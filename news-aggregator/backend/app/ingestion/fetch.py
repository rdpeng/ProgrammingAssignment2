"""RSS/Atom fetching. Every feed failure is caught and logged — a single dead
or slow feed must never take down a whole ingestion run.
"""
import logging

import feedparser
import httpx

from app.config import settings

logger = logging.getLogger("news_aggregator.fetch")

USER_AGENT = (
    "Mozilla/5.0 (compatible; BilingualNewsAggregator/1.0; "
    "+https://github.com/; personal-use RSS reader)"
)


def fetch_feed(feed_url: str) -> feedparser.FeedParserDict | None:
    """Fetch and parse one feed. Returns None on any failure."""
    try:
        resp = httpx.get(
            feed_url,
            headers={"User-Agent": USER_AGENT},
            timeout=settings.http_timeout_seconds,
            follow_redirects=True,
        )
        resp.raise_for_status()
        parsed = feedparser.parse(resp.content)
        if parsed.bozo and not parsed.entries:
            logger.warning("Feed %s parsed with errors and no entries: %s", feed_url, parsed.bozo_exception)
            return None
        return parsed
    except Exception as exc:
        logger.warning("Failed to fetch feed %s: %s", feed_url, exc)
        return None
