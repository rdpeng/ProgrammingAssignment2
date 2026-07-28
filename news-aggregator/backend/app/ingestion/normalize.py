"""Normalize a raw feedparser entry into the common item schema.

All datetimes in this app are stored and compared as naive UTC (SQLite does
not reliably round-trip tz-aware datetimes), so every value produced or
consumed here is UTC with tzinfo stripped, by convention.
"""
import calendar
from datetime import datetime, timezone


def parse_published(entry) -> datetime:
    for key in ("published_parsed", "updated_parsed"):
        struct = entry.get(key)
        if struct:
            return datetime.fromtimestamp(calendar.timegm(struct), tz=timezone.utc).replace(tzinfo=None)
    return datetime.now(timezone.utc).replace(tzinfo=None)


def extract_description(entry) -> str:
    if "summary" in entry:
        return entry.get("summary", "") or ""
    if "description" in entry:
        return entry.get("description", "") or ""
    content = entry.get("content")
    if content and isinstance(content, list):
        return content[0].get("value", "") or ""
    return ""


def normalize_entry(entry, default_language: str) -> dict | None:
    title = (entry.get("title") or "").strip()
    link = (entry.get("link") or "").strip()
    if not title or not link:
        return None
    return {
        "title": title,
        "url": link,
        "raw_description": extract_description(entry),
        "published_at": parse_published(entry),
        "language": default_language,
    }
