"""Loaders for the editable YAML config files (feeds, categories, macro-tags).

Kept dependency-free of the DB layer so config can be reloaded at any time
without a restart. Results are cached in-process, keyed on the file's mtime,
so editing a YAML file on disk is picked up on the next call automatically
without a manual cache-clear.
"""
from pathlib import Path

import yaml

from app.config import settings

_cache: dict[Path, tuple[float, object]] = {}


def _load_yaml(path: Path) -> dict:
    path = Path(path)
    mtime = path.stat().st_mtime
    cached = _cache.get(path)
    if cached and cached[0] == mtime:
        return cached[1]
    with open(path, encoding="utf-8") as f:
        data = yaml.safe_load(f) or {}
    _cache[path] = (mtime, data)
    return data


def load_feeds() -> list[dict]:
    return _load_yaml(settings.feeds_config_path).get("sources", [])


def load_categories() -> dict[str, dict]:
    return _load_yaml(settings.categories_config_path).get("categories", {})


def load_macro_tags() -> dict[str, dict]:
    return _load_yaml(settings.macro_tags_config_path).get("macro_tags", {})
