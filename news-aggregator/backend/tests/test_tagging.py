from app.processing.tagging import tag_item


def test_rbi_repo_rate_gets_expected_tags():
    tags = tag_item(
        "RBI hikes repo rate by 25 bps",
        "The Reserve Bank of India raised the repo rate citing inflation and crude oil prices.",
    )
    assert "india_economy_markets" in tags["category"]
    assert "rates" in tags["macro"]
    assert "crude" in tags["macro"]


def test_unrelated_headline_gets_no_macro_tags():
    tags = tag_item("Local school wins regional art competition", "Students celebrate their win.")
    assert tags["macro"] == []


def test_multiple_categories_can_match():
    tags = tag_item(
        "Parliament debates new GST reform bill amid inflation concerns",
        "Lok Sabha members discussed the fiscal deficit and GST changes.",
    )
    assert "india_politics_policy" in tags["category"]
    assert "india_economy_markets" in tags["category"]


def test_commodity_trading_headline_gets_tagged():
    tags = tag_item(
        "Gold futures rise on MCX as COMEX prices rally",
        "Commodity traders tracked open interest and spot price moves in bullion markets.",
    )
    assert "commodity_trading" in tags["category"]
    assert "metals" in tags["macro"]


def test_keyword_matching_is_word_boundary_aware_not_substring():
    # Regression test: "modi" (a keyword for PM Modi mentions) must not
    # match inside unrelated words like "commodity" ("com-modi-ty").
    tags = tag_item("Commodity prices rally across markets", "Traders eye commodity futures.")
    assert "india_politics_policy" not in tags["category"]
