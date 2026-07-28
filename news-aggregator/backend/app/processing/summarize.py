"""Short, original, copyright-safe summaries.

We never store or show full article text. The default (and always-available)
path is extractive: take the publisher's own RSS description (already a short
teaser, not the article body), strip markup, and hard-cap it at
`settings.summary_max_words` words.

An optional pluggable LLM path can rewrite that same short description into a
tighter original summary, still capped at the same word limit. If no provider
is configured, or the call fails for any reason, we silently fall back to the
extractive summary — ingestion must never block or fail because of this.
"""
import html
import re

from app.config import settings

_TAG_RE = re.compile(r"<[^>]+>")
_WS_RE = re.compile(r"\s+")


def clean_text(raw: str) -> str:
    if not raw:
        return ""
    text = _TAG_RE.sub(" ", raw)
    text = html.unescape(text)
    text = _WS_RE.sub(" ", text).strip()
    return text


def cap_words(text: str, max_words: int) -> str:
    words = text.split()
    if len(words) <= max_words:
        return text
    return " ".join(words[:max_words]).rstrip(",.;:") + "..."


def extractive_summary(raw_description: str, title: str) -> str:
    cleaned = clean_text(raw_description)
    if not cleaned:
        cleaned = title  # graceful fallback: show at least the headline
    return cap_words(cleaned, settings.summary_max_words)


def _llm_summary(title: str, cleaned_description: str) -> str | None:
    if settings.llm_summarizer_provider != "anthropic" or not settings.anthropic_api_key:
        return None
    try:
        import anthropic

        client = anthropic.Anthropic(api_key=settings.anthropic_api_key)
        prompt = (
            "Write a factual, neutral, original one-sentence news summary "
            f"(max {settings.summary_max_words} words, no speculation, no "
            "opinion) of this headline and blurb. Do not copy phrases "
            f"verbatim.\n\nHeadline: {title}\nBlurb: {cleaned_description}"
        )
        resp = client.messages.create(
            model=settings.anthropic_model,
            max_tokens=120,
            messages=[{"role": "user", "content": prompt}],
        )
        text = "".join(block.text for block in resp.content if hasattr(block, "text"))
        return cap_words(clean_text(text), settings.summary_max_words) or None
    except Exception:
        return None


def summarize(title: str, raw_description: str) -> str:
    cleaned = clean_text(raw_description)
    llm_result = _llm_summary(title, cleaned) if cleaned else None
    if llm_result:
        return llm_result
    return extractive_summary(raw_description, title)
