"""Orchestrates one full ingestion run: fetch all feeds -> normalize -> dedupe
-> tag -> summarize -> persist. Safe to call repeatedly (idempotent on URL).
"""
import logging
from concurrent.futures import ThreadPoolExecutor, as_completed

from sqlalchemy import select
from sqlalchemy.orm import Session

from app.config import settings
from app.ingestion.fetch import fetch_feed
from app.ingestion.normalize import normalize_entry
from app.ingestion.optional_apis import OPTIONAL_PROVIDERS
from app.models import Item, Source, Tag
from app.processing.dedup import canonicalize_url, find_or_create_cluster
from app.processing.summarize import summarize
from app.processing.tagging import tag_item
from app.yaml_config import load_feeds

logger = logging.getLogger("news_aggregator.pipeline")


def sync_sources(db: Session) -> list[Source]:
    """Create/update Source rows from feeds.yaml (upsert by feed_url)."""
    configured = load_feeds()
    sources = []
    for entry in configured:
        feed_url = entry["feed_url"]
        source = db.scalar(select(Source).where(Source.feed_url == feed_url))
        if source is None:
            source = Source(feed_url=feed_url)
            db.add(source)
        source.name = entry.get("name", feed_url)
        source.url = entry.get("url", "")
        source.language = entry.get("language", "en")
        source.category = entry.get("category", "")
        source.enabled = entry.get("enabled", True)
        sources.append(source)
    db.commit()
    return sources


def sync_optional_api_source(db: Session, provider_key: str, display_name: str) -> Source:
    """Upsert a virtual Source row representing an optional free-tier API."""
    feed_url = f"optional-api://{provider_key}"
    source = db.scalar(select(Source).where(Source.feed_url == feed_url))
    if source is None:
        source = Source(feed_url=feed_url)
        db.add(source)
    source.name = display_name
    source.url = ""
    source.language = "en"
    source.category = "global_macro"
    source.enabled = True
    db.commit()
    return source


def _persist_item(db: Session, source: Source, normalized: dict) -> tuple[Item | None, bool, bool]:
    """Returns (item, is_new, cluster_created)."""
    url_canonical = canonicalize_url(normalized["url"])
    existing = db.scalar(select(Item).where(Item.url_canonical == url_canonical))
    if existing is not None:
        return existing, False, False

    summary = summarize(normalized["title"], normalized["raw_description"])

    item = Item(
        source_id=source.id,
        title=normalized["title"],
        url=normalized["url"],
        url_canonical=url_canonical,
        summary=summary,
        raw_description=normalized["raw_description"][:2000],
        language=normalized["language"],
        published_at=normalized["published_at"],
    )
    db.add(item)
    db.flush()  # assign item.id

    tags = tag_item(normalized["title"], summary)
    for category_key in tags["category"]:
        db.add(Tag(item_id=item.id, tag=category_key, tag_type="category"))
    for macro_key in tags["macro"]:
        db.add(Tag(item_id=item.id, tag=macro_key, tag_type="macro"))

    _, cluster_created = find_or_create_cluster(db, item)
    db.commit()
    return item, True, cluster_created


def _persist_batch(db: Session, source: Source, normalized_items: list[dict], stats: dict) -> None:
    for normalized in normalized_items:
        stats["items_fetched"] += 1
        try:
            _, is_new, cluster_created = _persist_item(db, source, normalized)
        except Exception:
            db.rollback()
            logger.exception("Failed to persist item from %s", source.name)
            continue
        if is_new:
            stats["items_new"] += 1
            if cluster_created:
                stats["clusters_created"] += 1
            else:
                stats["clusters_merged"] += 1


def run_ingestion(db: Session) -> dict:
    sources = [s for s in sync_sources(db) if s.enabled]

    stats = {
        "sources_polled": len(sources),
        "sources_failed": 0,
        "items_fetched": 0,
        "items_new": 0,
        "clusters_created": 0,
        "clusters_merged": 0,
        "failed_sources": [],
    }

    fetched_by_source: dict[int, object] = {}
    with ThreadPoolExecutor(max_workers=settings.fetch_concurrency) as pool:
        future_to_source = {pool.submit(fetch_feed, s.feed_url): s for s in sources}
        for future in as_completed(future_to_source):
            source = future_to_source[future]
            parsed = future.result()
            if parsed is None:
                stats["sources_failed"] += 1
                stats["failed_sources"].append(source.name)
            else:
                fetched_by_source[source.id] = parsed

    for source in sources:
        parsed = fetched_by_source.get(source.id)
        if parsed is None:
            continue
        normalized_items = [
            n for n in (normalize_entry(entry, source.language) for entry in parsed.entries)
            if n is not None
        ]
        _persist_batch(db, source, normalized_items, stats)

    # Optional free-tier news APIs — no-ops unless their API key env var is set.
    for provider_key, (fetch_fn, display_name) in OPTIONAL_PROVIDERS.items():
        items = fetch_fn()
        if items is None:
            continue  # key absent, or the call failed and was already logged
        source = sync_optional_api_source(db, provider_key, display_name)
        stats["sources_polled"] += 1
        _persist_batch(db, source, items, stats)

    return stats
