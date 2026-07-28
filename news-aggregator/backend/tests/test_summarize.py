from app.config import settings
from app.processing.summarize import cap_words, clean_text, extractive_summary


def test_clean_text_strips_html_and_entities():
    assert clean_text("<p>Hello &amp; welcome</p>") == "Hello & welcome"


def test_cap_words_under_limit_unchanged():
    text = "short summary"
    assert cap_words(text, 40) == text


def test_cap_words_truncates_and_marks_ellipsis():
    words = " ".join(f"word{i}" for i in range(60))
    capped = cap_words(words, 40)
    assert len(capped.split()) <= 41  # 40 words + possible "..." token
    assert capped.endswith("...")


def test_extractive_summary_never_exceeds_configured_word_cap():
    long_description = "<p>" + " ".join(["breaking"] * 100) + "</p>"
    summary = extractive_summary(long_description, "Fallback title")
    assert len(summary.split()) <= settings.summary_max_words + 1


def test_extractive_summary_falls_back_to_title_when_description_empty():
    summary = extractive_summary("", "Only a headline here")
    assert summary == "Only a headline here"
