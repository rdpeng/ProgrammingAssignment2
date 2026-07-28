"""Application settings, all overridable via environment variables (.env)."""
from pathlib import Path

from pydantic_settings import BaseSettings, SettingsConfigDict

BASE_DIR = Path(__file__).resolve().parent.parent
CONFIG_DIR = BASE_DIR / "config"


class Settings(BaseSettings):
    model_config = SettingsConfigDict(env_file=".env", extra="ignore")

    # Database
    database_url: str = f"sqlite:///{BASE_DIR / 'data' / 'news.db'}"

    # Editable config files (feeds / categories / macro-tags)
    feeds_config_path: Path = CONFIG_DIR / "feeds.yaml"
    categories_config_path: Path = CONFIG_DIR / "categories.yaml"
    macro_tags_config_path: Path = CONFIG_DIR / "macro_tags.yaml"

    # Ingestion
    poll_interval_minutes: int = 15
    http_timeout_seconds: float = 15.0
    fetch_concurrency: int = 8

    # Dedup (rapidfuzz token_set_ratio, 0-100; see processing/dedup.py docstring)
    dedup_title_similarity_threshold: int = 85  # near-identical / wire-syndicated titles
    dedup_combined_similarity_threshold: int = 58  # paraphrased cross-outlet coverage
    dedup_window_hours: int = 72

    # Summarization
    summary_max_words: int = 40
    llm_summarizer_provider: str = "none"  # "none" | "anthropic"
    anthropic_api_key: str | None = None
    anthropic_model: str = "claude-haiku-4-5-20251001"

    # Translation (pluggable; degrades gracefully if unavailable)
    translation_provider: str = "google_free"  # "none" | "google_free" | "libretranslate"
    libretranslate_url: str | None = None
    libretranslate_api_key: str | None = None
    translate_timeout_seconds: float = 8.0

    # Optional free-tier news API aggregators (never required)
    gnews_api_key: str | None = None
    newsdata_api_key: str | None = None
    currents_api_key: str | None = None


settings = Settings()
