from datetime import datetime, timedelta

import pytest
from fastapi.testclient import TestClient

from app.db import Base, SessionLocal, engine
from app.main import app
from app.models import Item, Source, Tag
from app.processing.dedup import canonicalize_url, find_or_create_cluster


@pytest.fixture()
def client():
    Base.metadata.create_all(bind=engine)
    with TestClient(app) as c:
        yield c
    Base.metadata.drop_all(bind=engine)


def _seed_items():
    db = SessionLocal()
    try:
        source = Source(name="Test Wire", feed_url="http://example.com/wire.xml", language="en")
        db.add(source)
        db.commit()

        url = "http://example.com/rbi-story"
        item = Item(
            source_id=source.id,
            title="RBI hikes repo rate by 25 bps",
            url=url,
            url_canonical=canonicalize_url(url),
            summary="The RBI raised the repo rate citing inflation.",
            language="en",
            published_at=datetime.utcnow(),
        )
        db.add(item)
        db.flush()
        db.add(Tag(item_id=item.id, tag="rates", tag_type="macro"))
        db.add(Tag(item_id=item.id, tag="india_economy_markets", tag_type="category"))
        find_or_create_cluster(db, item)
        db.commit()
        return item.id
    finally:
        db.close()


def test_health(client):
    resp = client.get("/api/health")
    assert resp.status_code == 200
    assert resp.json() == {"status": "ok"}


def test_meta_endpoints_return_seeded_taxonomy(client):
    categories = client.get("/api/meta/categories").json()
    macro_tags = client.get("/api/meta/macro-tags").json()
    assert any(c["key"] == "india_economy_markets" for c in categories)
    assert any(m["key"] == "rates" for m in macro_tags)


def test_items_list_and_macro_filter(client):
    _seed_items()

    all_items = client.get("/api/items").json()
    assert len(all_items) == 1
    assert all_items[0]["title"].startswith("RBI hikes")
    assert all_items[0]["source_count"] == 1

    matching = client.get("/api/items?macro=rates").json()
    assert len(matching) == 1

    non_matching = client.get("/api/items?macro=crude").json()
    assert non_matching == []


def test_items_since_hours_filter_excludes_old_items(client):
    db = SessionLocal()
    source = Source(name="Old Wire", feed_url="http://example.com/old.xml", language="en")
    db.add(source)
    db.commit()
    old_item = Item(
        source_id=source.id,
        title="An old story from last week",
        url="http://example.com/old-story",
        url_canonical=canonicalize_url("http://example.com/old-story"),
        summary="Old news.",
        language="en",
        published_at=datetime.utcnow() - timedelta(days=10),
    )
    db.add(old_item)
    db.flush()
    find_or_create_cluster(db, old_item)
    db.commit()
    db.close()

    recent = client.get("/api/items?since_hours=24").json()
    assert recent == []

    everything = client.get("/api/items").json()
    assert len(everything) == 1


def test_tag_update_endpoint_add_and_remove(client):
    item_id = _seed_items()

    resp = client.post(f"/api/items/{item_id}/tags", json={"tag": "custom", "tag_type": "topic", "action": "add"})
    assert resp.status_code == 200
    assert any(t["tag"] == "custom" for t in resp.json())

    resp = client.post(f"/api/items/{item_id}/tags", json={"tag": "custom", "tag_type": "topic", "action": "remove"})
    assert resp.status_code == 200
    assert not any(t["tag"] == "custom" for t in resp.json())


def test_cluster_sources_endpoint(client):
    item_id = _seed_items()
    items = client.get("/api/items").json()
    cluster_id = items[0]["cluster_id"]

    resp = client.get(f"/api/items/{cluster_id}/sources")
    assert resp.status_code == 200
    members = resp.json()
    assert len(members) == 1
    assert members[0]["item_id"] == item_id
