from datetime import datetime, timezone

from sqlalchemy import (
    DateTime,
    Float,
    ForeignKey,
    Integer,
    String,
    Text,
    UniqueConstraint,
)
from sqlalchemy.orm import Mapped, mapped_column, relationship

from app.db import Base


def utcnow() -> datetime:
    return datetime.now(timezone.utc).replace(tzinfo=None)


class Source(Base):
    __tablename__ = "sources"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    name: Mapped[str] = mapped_column(String(200))
    url: Mapped[str] = mapped_column(String(500), default="")
    feed_url: Mapped[str] = mapped_column(String(500), unique=True)
    language: Mapped[str] = mapped_column(String(8), default="en")
    category: Mapped[str] = mapped_column(String(64), default="")
    enabled: Mapped[bool] = mapped_column(default=True)

    items: Mapped[list["Item"]] = relationship(back_populates="source")


class Item(Base):
    __tablename__ = "items"
    __table_args__ = (UniqueConstraint("url_canonical", name="uq_items_url_canonical"),)

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    source_id: Mapped[int] = mapped_column(ForeignKey("sources.id"))
    title: Mapped[str] = mapped_column(String(1000))
    url: Mapped[str] = mapped_column(String(2000))
    url_canonical: Mapped[str] = mapped_column(String(2000), index=True)
    summary: Mapped[str] = mapped_column(Text, default="")
    raw_description: Mapped[str] = mapped_column(Text, default="")
    language: Mapped[str] = mapped_column(String(8), default="en")
    published_at: Mapped[datetime] = mapped_column(DateTime, index=True)
    fetched_at: Mapped[datetime] = mapped_column(DateTime, default=utcnow)

    # On-demand translation cache (populated lazily via /translate endpoint)
    translated_title: Mapped[str | None] = mapped_column(Text, nullable=True)
    translated_summary: Mapped[str | None] = mapped_column(Text, nullable=True)
    translated_language: Mapped[str | None] = mapped_column(String(8), nullable=True)

    source: Mapped["Source"] = relationship(back_populates="items")
    tags: Mapped[list["Tag"]] = relationship(back_populates="item", cascade="all, delete-orphan")
    cluster_links: Mapped[list["ClusterItem"]] = relationship(
        back_populates="item", cascade="all, delete-orphan"
    )


class Tag(Base):
    __tablename__ = "tags"
    __table_args__ = (UniqueConstraint("item_id", "tag", "tag_type", name="uq_tags_item_tag_type"),)

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    item_id: Mapped[int] = mapped_column(ForeignKey("items.id"))
    tag: Mapped[str] = mapped_column(String(64), index=True)
    tag_type: Mapped[str] = mapped_column(String(16))  # "category" | "macro" | "topic"

    item: Mapped["Item"] = relationship(back_populates="tags")


class Cluster(Base):
    __tablename__ = "clusters"

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    representative_item_id: Mapped[int | None] = mapped_column(
        ForeignKey("items.id"), nullable=True
    )
    created_at: Mapped[datetime] = mapped_column(DateTime, default=utcnow)

    representative_item: Mapped["Item | None"] = relationship(foreign_keys=[representative_item_id])
    item_links: Mapped[list["ClusterItem"]] = relationship(
        back_populates="cluster", cascade="all, delete-orphan"
    )


class ClusterItem(Base):
    __tablename__ = "cluster_items"
    __table_args__ = (UniqueConstraint("cluster_id", "item_id", name="uq_cluster_items"),)

    id: Mapped[int] = mapped_column(Integer, primary_key=True)
    cluster_id: Mapped[int] = mapped_column(ForeignKey("clusters.id"), index=True)
    item_id: Mapped[int] = mapped_column(ForeignKey("items.id"), index=True, unique=True)

    cluster: Mapped["Cluster"] = relationship(back_populates="item_links", foreign_keys=[cluster_id])
    item: Mapped["Item"] = relationship(back_populates="cluster_links")
