"""Keyword-rule categorization + macro-impact tagging.

An item can match multiple categories and multiple macro tags. Matching is a
case-insensitive, word-boundary-aware search of each rule's keywords against
the item's title + summary text (e.g. the keyword "modi" matches "Modi" but
not the "modi" inside "commodity"). This is intentionally simple/transparent
so the YAML rule files stay easy to edit without touching code.
"""
import re

from app.yaml_config import load_categories, load_macro_tags


def _keyword_pattern(keyword: str) -> re.Pattern:
    # \b requires a word character on the matching side, so this also works
    # correctly for multi-word phrases like "spot price" and for keywords
    # containing regex-special characters (e.g. "opec+"), which re.escape
    # neutralizes.
    return re.compile(r"\b" + re.escape(keyword.lower().strip()) + r"\b")


def _match_rules(text: str, rules: dict[str, dict]) -> list[str]:
    haystack = text.lower()
    matched = []
    for key, rule in rules.items():
        for keyword in rule.get("keywords", []):
            if _keyword_pattern(keyword).search(haystack):
                matched.append(key)
                break
    return matched


def tag_item(title: str, summary: str) -> dict[str, list[str]]:
    """Return {"category": [...], "macro": [...]} tag keys for an item."""
    text = f"{title} {summary}"
    categories = _match_rules(text, load_categories())
    macro = _match_rules(text, load_macro_tags())
    return {"category": categories, "macro": macro}
