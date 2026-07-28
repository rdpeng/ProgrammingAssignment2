from datetime import datetime, timedelta

from app.models import Item, Source
from app.processing.dedup import canonicalize_url, find_or_create_cluster


def test_canonicalize_url_strips_tracking_params_and_www():
    a = canonicalize_url("https://www.example.com/news/story/?utm_source=twitter&id=5")
    b = canonicalize_url("https://example.com/news/story?id=5")
    assert a == b


def test_canonicalize_url_ignores_trailing_slash():
    assert canonicalize_url("https://example.com/a/") == canonicalize_url("https://example.com/a")


def _make_source(db_session, name="Test Source"):
    source = Source(name=name, feed_url=f"http://example.com/{name}.xml", language="en")
    db_session.add(source)
    db_session.commit()
    return source


def _make_item(db_session, source, title, summary, published_at, url):
    item = Item(
        source_id=source.id,
        title=title,
        url=url,
        url_canonical=canonicalize_url(url),
        summary=summary,
        language="en",
        published_at=published_at,
    )
    db_session.add(item)
    db_session.flush()
    return item


def test_paraphrased_cross_outlet_headlines_merge_into_one_cluster(db_session):
    source_a = _make_source(db_session, "A")
    source_b = _make_source(db_session, "B")
    now = datetime.utcnow()

    item1 = _make_item(
        db_session, source_a,
        "RBI hikes repo rate by 25 bps to tame inflation",
        "The Reserve Bank of India's Monetary Policy Committee raised the repo rate citing "
        "persistent inflation concerns and global crude oil price pressure on the rupee.",
        now, "http://a.example.com/rbi-1",
    )
    cluster1, created1 = find_or_create_cluster(db_session, item1)
    assert created1 is True

    item2 = _make_item(
        db_session, source_b,
        "RBI raises repo rate 25 basis points to curb inflation",
        "India's central bank increased the benchmark repo rate as inflation stayed above "
        "target, with crude oil prices also in focus for markets.",
        now + timedelta(minutes=5), "http://b.example.com/rbi-2",
    )
    cluster2, created2 = find_or_create_cluster(db_session, item2)

    assert created2 is False
    assert cluster2.id == cluster1.id


def test_unrelated_stories_stay_in_separate_clusters(db_session):
    source = _make_source(db_session)
    now = datetime.utcnow()

    item1 = _make_item(
        db_session, source, "RBI hikes repo rate by 25 bps to tame inflation",
        "The Reserve Bank of India raised rates citing inflation.",
        now, "http://example.com/story-1",
    )
    find_or_create_cluster(db_session, item1)

    item2 = _make_item(
        db_session, source, "ISRO successfully launches new communication satellite",
        "ISRO's latest satellite mission boosts communication infrastructure.",
        now, "http://example.com/story-2",
    )
    cluster2, created2 = find_or_create_cluster(db_session, item2)

    assert created2 is True


def test_same_url_with_different_tracking_params_is_never_duplicated(db_session):
    # url_canonical has a DB-level UNIQUE constraint, and _persist_item checks
    # for an existing row by canonical URL before inserting — so re-ingesting
    # the same story (e.g. re-shared with a different utm_source) must return
    # the existing item rather than create a second row.
    from app.ingestion.pipeline import _persist_item

    source = _make_source(db_session)
    now = datetime.utcnow()
    normalized = {
        "title": "Some headline",
        "url": "http://example.com/story?utm_source=x",
        "raw_description": "Some summary",
        "published_at": now,
        "language": "en",
    }
    item1, is_new1, _ = _persist_item(db_session, source, normalized)
    assert is_new1 is True

    normalized["url"] = "http://example.com/story?utm_source=y"  # same story, different tracking param
    item2, is_new2, _ = _persist_item(db_session, source, normalized)

    assert is_new2 is False
    assert item2.id == item1.id
