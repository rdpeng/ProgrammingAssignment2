from datetime import datetime, timedelta, timezone

from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy import exists, func, or_, select
from sqlalchemy.orm import Session

from app.db import get_db
from app.models import Cluster, ClusterItem, Item, Source, Tag
from app.schemas import ClusterMemberOut, FeedCardOut, TagOut, TagUpdateRequest

router = APIRouter(prefix="/api/items", tags=["items"])


def _source_count_subq(item_id_col):
    return (
        select(func.count(ClusterItem.id))
        .select_from(ClusterItem)
        .join(Cluster, Cluster.id == ClusterItem.cluster_id)
        .where(Cluster.representative_item_id == item_id_col)
        .correlate(Item)
        .scalar_subquery()
    )


@router.get("", response_model=list[FeedCardOut])
def list_items(
    db: Session = Depends(get_db),
    category: list[str] | None = Query(None),
    macro: list[str] | None = Query(None),
    language: str | None = None,
    source_id: int | None = None,
    since_hours: int | None = Query(None, ge=1, le=24 * 30),
    q: str | None = None,
    limit: int = Query(50, ge=1, le=200),
    offset: int = Query(0, ge=0),
):
    """One card per story (cluster representative), newest first."""
    stmt = (
        select(Item)
        .join(Cluster, Cluster.representative_item_id == Item.id)
        .join(Source, Source.id == Item.source_id)
    )

    if language:
        stmt = stmt.where(Item.language == language)
    if source_id:
        stmt = stmt.where(Item.source_id == source_id)
    if since_hours:
        cutoff = datetime.now(timezone.utc).replace(tzinfo=None) - timedelta(hours=since_hours)
        stmt = stmt.where(Item.published_at >= cutoff)
    if q:
        like = f"%{q.lower()}%"
        stmt = stmt.where(
            or_(func.lower(Item.title).like(like), func.lower(Item.summary).like(like))
        )
    if category:
        stmt = stmt.where(
            exists().where(
                Tag.item_id == Item.id, Tag.tag_type == "category", Tag.tag.in_(category)
            )
        )
    if macro:
        stmt = stmt.where(
            exists().where(Tag.item_id == Item.id, Tag.tag_type == "macro", Tag.tag.in_(macro))
        )

    stmt = stmt.order_by(Item.published_at.desc()).offset(offset).limit(limit)
    items = db.scalars(stmt).all()

    results = []
    for item in items:
        cluster = db.scalar(select(Cluster).where(Cluster.representative_item_id == item.id))
        source_count = db.scalar(
            select(func.count(ClusterItem.id)).where(ClusterItem.cluster_id == cluster.id)
        ) if cluster else 1
        tags = db.scalars(select(Tag).where(Tag.item_id == item.id)).all()
        results.append(
            FeedCardOut(
                cluster_id=cluster.id if cluster else item.id,
                item_id=item.id,
                title=item.title,
                url=item.url,
                summary=item.summary,
                source_name=item.source.name,
                source_id=item.source_id,
                language=item.language,
                published_at=item.published_at,
                fetched_at=item.fetched_at,
                tags=[TagOut.model_validate(t) for t in tags],
                source_count=source_count or 1,
                translated_title=item.translated_title,
                translated_summary=item.translated_summary,
                translated_language=item.translated_language,
            )
        )
    return results


@router.get("/{cluster_id}/sources", response_model=list[ClusterMemberOut])
def cluster_sources(cluster_id: int, db: Session = Depends(get_db)):
    cluster = db.get(Cluster, cluster_id)
    if cluster is None:
        raise HTTPException(status_code=404, detail="Cluster not found")
    links = db.scalars(select(ClusterItem).where(ClusterItem.cluster_id == cluster_id)).all()
    out = []
    for link in links:
        item = db.get(Item, link.item_id)
        if item is None:
            continue
        out.append(
            ClusterMemberOut(
                item_id=item.id,
                title=item.title,
                url=item.url,
                source_name=item.source.name,
                language=item.language,
                published_at=item.published_at,
            )
        )
    out.sort(key=lambda m: m.published_at)
    return out


@router.post("/{item_id}/tags", response_model=list[TagOut])
def update_tags(item_id: int, body: TagUpdateRequest, db: Session = Depends(get_db)):
    item = db.get(Item, item_id)
    if item is None:
        raise HTTPException(status_code=404, detail="Item not found")

    if body.action == "add":
        existing = db.scalar(
            select(Tag).where(
                Tag.item_id == item_id, Tag.tag == body.tag, Tag.tag_type == body.tag_type
            )
        )
        if existing is None:
            db.add(Tag(item_id=item_id, tag=body.tag, tag_type=body.tag_type))
            db.commit()
    elif body.action == "remove":
        existing = db.scalar(
            select(Tag).where(
                Tag.item_id == item_id, Tag.tag == body.tag, Tag.tag_type == body.tag_type
            )
        )
        if existing is not None:
            db.delete(existing)
            db.commit()
    else:
        raise HTTPException(status_code=400, detail="action must be 'add' or 'remove'")

    tags = db.scalars(select(Tag).where(Tag.item_id == item_id)).all()
    return [TagOut.model_validate(t) for t in tags]
