"""Pluggable EN<->HI translation.

Translation is always on-demand (never blocks ingestion) and always
best-effort: any failure (network, missing provider, timeout) returns None so
the caller can fall back to showing the original-language text untranslated.

Providers:
  - "none"           : translation disabled, always returns None
  - "google_free"     : deep-translator's free, keyless Google Translate
                        endpoint (default; no API key, no cost, best-effort)
  - "libretranslate"  : self-hosted or public LibreTranslate instance
                        (set LIBRETRANSLATE_URL, optional API key)
"""
from app.config import settings

_LANG_MAP = {"en": "en", "hi": "hi"}


def _google_free(text: str, source: str, target: str) -> str | None:
    try:
        from deep_translator import GoogleTranslator

        result = GoogleTranslator(source=source, target=target).translate(text)
        return result or None
    except Exception:
        return None


def _libretranslate(text: str, source: str, target: str) -> str | None:
    if not settings.libretranslate_url:
        return None
    try:
        import httpx

        payload = {"q": text, "source": source, "target": target, "format": "text"}
        if settings.libretranslate_api_key:
            payload["api_key"] = settings.libretranslate_api_key
        resp = httpx.post(
            settings.libretranslate_url,
            json=payload,
            timeout=settings.translate_timeout_seconds,
        )
        resp.raise_for_status()
        return resp.json().get("translatedText") or None
    except Exception:
        return None


def translate_text(text: str, source_lang: str, target_lang: str) -> str | None:
    if not text or source_lang == target_lang:
        return text
    source = _LANG_MAP.get(source_lang, source_lang)
    target = _LANG_MAP.get(target_lang, target_lang)

    provider = settings.translation_provider
    if provider == "google_free":
        return _google_free(text, source, target)
    if provider == "libretranslate":
        return _libretranslate(text, source, target)
    return None


def translate_item(title: str, summary: str, source_lang: str, target_lang: str) -> tuple[str, str] | None:
    """Translate both title and summary; returns None if either fails."""
    t_title = translate_text(title, source_lang, target_lang)
    t_summary = translate_text(summary, source_lang, target_lang)
    if t_title is None or t_summary is None:
        return None
    return t_title, t_summary
