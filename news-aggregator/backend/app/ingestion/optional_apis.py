"""Optional free-tier news API adapters (GNews, NewsData.io, Currents).

None of these are required — the app runs on RSS alone. Each adapter is only
called if its API key env var is set, and any failure (bad key, quota
exhausted, network error, unexpected response shape) is caught and logged as
a skip, never raised, so ingestion always degrades gracefully to
RSS-only when a key is missing or a provider is unavailable.

Each adapter returns a list of dicts in the same normalized shape produced
by `app.ingestion.normalize.normalize_entry`, so downstream code (dedup,
tagging, summarization) treats these exactly like RSS items.
"""
import logging
from datetime import datetime, timezone

import httpx

from app.config import settings
from app.ingestion.fetch import USER_AGENT

logger = logging.getLogger("news_aggregator.optional_apis")


def _parse_iso(value: str) -> datetime:
    try:
        cleaned = value.replace("Z", "+00:00")
        dt = datetime.fromisoformat(cleaned)
        if dt.tzinfo is None:
            dt = dt.replace(tzinfo=timezone.utc)
        return dt.astimezone(timezone.utc).replace(tzinfo=None)
    except Exception:
        return datetime.now(timezone.utc).replace(tzinfo=None)


def _get_json(url: str, params: dict) -> dict | None:
    resp = httpx.get(
        url, params=params, headers={"User-Agent": USER_AGENT},
        timeout=settings.http_timeout_seconds,
    )
    resp.raise_for_status()
    return resp.json()


def fetch_gnews() -> list[dict] | None:
    """https://gnews.io free tier: 100 requests/day, no card required."""
    if not settings.gnews_api_key:
        return None
    try:
        data = _get_json(
            "https://gnews.io/api/v4/top-headlines",
            {"apikey": settings.gnews_api_key, "lang": "en", "country": "in", "max": 25},
        )
        items = []
        for article in data.get("articles", []):
            if not article.get("title") or not article.get("url"):
                continue
            items.append({
                "title": article["title"],
                "url": article["url"],
                "raw_description": article.get("description") or article.get("content") or "",
                "published_at": _parse_iso(article.get("publishedAt", "")),
                "language": "en",
            })
        return items
    except Exception as exc:
        logger.warning("GNews fetch failed, skipping: %s", exc)
        return None


def fetch_newsdata() -> list[dict] | None:
    """https://newsdata.io free tier: 200 requests/day, no card required."""
    if not settings.newsdata_api_key:
        return None
    try:
        data = _get_json(
            "https://newsdata.io/api/1/news",
            {"apikey": settings.newsdata_api_key, "country": "in", "language": "en"},
        )
        items = []
        for article in data.get("results", []):
            if not article.get("title") or not article.get("link"):
                continue
            items.append({
                "title": article["title"],
                "url": article["link"],
                "raw_description": article.get("description") or "",
                "published_at": _parse_iso((article.get("pubDate") or "").replace(" ", "T")),
                "language": "en",
            })
        return items
    except Exception as exc:
        logger.warning("NewsData.io fetch failed, skipping: %s", exc)
        return None


def fetch_currents() -> list[dict] | None:
    """https://currentsapi.services free tier: 20 requests/day, no card required."""
    if not settings.currents_api_key:
        return None
    try:
        data = _get_json(
            "https://api.currentsapi.services/v1/latest-news",
            {"apiKey": settings.currents_api_key, "language": "en"},
        )
        items = []
        for article in data.get("news", []):
            if not article.get("title") or not article.get("url"):
                continue
            items.append({
                "title": article["title"],
                "url": article["url"],
                "raw_description": article.get("description") or "",
                "published_at": _parse_iso(article.get("published", "")),
                "language": "en",
            })
        return items
    except Exception as exc:
        logger.warning("Currents fetch failed, skipping: %s", exc)
        return None


# name -> (fetch function, display name for the virtual Source row)
OPTIONAL_PROVIDERS = {
    "gnews": (fetch_gnews, "GNews (free tier)"),
    "newsdata": (fetch_newsdata, "NewsData.io (free tier)"),
    "currents": (fetch_currents, "Currents (free tier)"),
}
