"""Near-duplicate story clustering.

Two signals decide whether a new item joins an existing cluster:
  1. URL canonicalization (see `canonicalize_url`) — stripping tracking
     params/fragments so the same article re-shared with a different query
     string is recognized as the same item before it's ever inserted
     (`url_canonical` is unique in the DB; see pipeline._persist_item).
  2. Title + summary similarity — rapidfuzz token_set_ratio, checked two
     ways: against titles alone (high threshold, for near-identical /
     wire-syndicated headlines) and against title+summary combined (a
     lower, separately-tuned threshold). Outlets paraphrase headlines
     heavily ("hikes" vs "raises", "bps" vs "basis points"), so title text
     alone under-matches real cross-outlet coverage of the same story; the
     summary text shares far more vocabulary (named entities, figures) and
     separates true matches from unrelated stories much more reliably.
An item joining via either signal is treated as the same story.

The earliest-published item in a cluster is kept as the representative
(what the feed shows); the rest surface via the "N sources" expander.
"""
from datetime import timedelta
from urllib.parse import parse_qsl, urlencode, urlparse, urlunparse

from rapidfuzz import fuzz
from sqlalchemy import select
from sqlalchemy.orm import Session

from app.config import settings
from app.models import Cluster, ClusterItem, Item

_TRACKING_PARAMS = {
    "utm_source", "utm_medium", "utm_campaign", "utm_term", "utm_content",
    "fbclid", "gclid", "ref", "cmpid", "amp",
}


def canonicalize_url(url: str) -> str:
    parsed = urlparse(url.strip())
    query = [
        (k, v) for k, v in parse_qsl(parsed.query, keep_blank_values=True)
        if k.lower() not in _TRACKING_PARAMS
    ]
    path = parsed.path.rstrip("/") or "/"
    normalized = parsed._replace(
        scheme=parsed.scheme.lower(),
        netloc=parsed.netloc.lower().removeprefix("www."),
        path=path,
        query=urlencode(sorted(query)),
        fragment="",
    )
    return urlunparse(normalized)


def find_or_create_cluster(db: Session, item: Item) -> tuple[Cluster, bool]:
    """Attach `item` to a matching cluster, or create a new one.

    Returns (cluster, created_new). `item` must already be flushed/have an id.
    """
    window_start = item.published_at - timedelta(hours=settings.dedup_window_hours)
    window_end = item.published_at + timedelta(hours=settings.dedup_window_hours)

    candidates = db.scalars(
        select(Item)
        .where(
            Item.published_at >= window_start,
            Item.published_at <= window_end,
            Item.id != item.id,
        )
        .order_by(Item.published_at.desc())
        .limit(500)
    ).all()

    item_title = item.title.lower()
    item_combined = f"{item.title} {item.summary}".lower()

    best_match: Item | None = None
    best_confidence = 0.0  # margin above whichever threshold matched
    for candidate in candidates:
        title_score = fuzz.token_set_ratio(item_title, candidate.title.lower())
        combined_score = fuzz.token_set_ratio(
            item_combined, f"{candidate.title} {candidate.summary}".lower()
        )
        is_match = (
            title_score >= settings.dedup_title_similarity_threshold
            or combined_score >= settings.dedup_combined_similarity_threshold
        )
        if not is_match:
            continue
        confidence = max(
            title_score - settings.dedup_title_similarity_threshold,
            combined_score - settings.dedup_combined_similarity_threshold,
        )
        if confidence > best_confidence:
            best_confidence = confidence
            best_match = candidate

    if best_match is not None:
        existing_link = db.scalar(
            select(ClusterItem).where(ClusterItem.item_id == best_match.id)
        )
        if existing_link:
            cluster = db.get(Cluster, existing_link.cluster_id)
        else:
            # best_match wasn't clustered yet (shouldn't normally happen) — create one
            cluster = Cluster(representative_item_id=best_match.id)
            db.add(cluster)
            db.flush()
            db.add(ClusterItem(cluster_id=cluster.id, item_id=best_match.id))

        db.add(ClusterItem(cluster_id=cluster.id, item_id=item.id))

        # keep the earliest-published item as representative
        rep = cluster.representative_item
        if rep is None or item.published_at < rep.published_at:
            cluster.representative_item_id = item.id
        db.flush()
        return cluster, False

    cluster = Cluster(representative_item_id=item.id)
    db.add(cluster)
    db.flush()
    db.add(ClusterItem(cluster_id=cluster.id, item_id=item.id))
    db.flush()
    return cluster, True
