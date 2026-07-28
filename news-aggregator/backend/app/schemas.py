from datetime import datetime

from pydantic import BaseModel, ConfigDict


class SourceOut(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    id: int
    name: str
    url: str
    language: str
    category: str


class TagOut(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    tag: str
    tag_type: str


class ClusterMemberOut(BaseModel):
    model_config = ConfigDict(from_attributes=True)

    item_id: int
    title: str
    url: str
    source_name: str
    language: str
    published_at: datetime


class FeedCardOut(BaseModel):
    cluster_id: int
    item_id: int
    title: str
    url: str
    summary: str
    source_name: str
    source_id: int
    language: str
    published_at: datetime
    fetched_at: datetime
    tags: list[TagOut]
    source_count: int
    translated_title: str | None = None
    translated_summary: str | None = None
    translated_language: str | None = None


class TranslateResponse(BaseModel):
    item_id: int
    translated_title: str
    translated_summary: str
    translated_language: str
    provider: str


class TagUpdateRequest(BaseModel):
    tag: str
    tag_type: str = "topic"
    action: str = "add"  # "add" | "remove"


class MetaTagOut(BaseModel):
    key: str
    label: str


class IngestRunResult(BaseModel):
    sources_polled: int
    sources_failed: int
    items_fetched: int
    items_new: int
    clusters_created: int
    clusters_merged: int
    failed_sources: list[str]
