from fastapi import APIRouter

from app.schemas import MetaTagOut
from app.yaml_config import load_categories, load_macro_tags

router = APIRouter(prefix="/api/meta", tags=["meta"])


@router.get("/categories", response_model=list[MetaTagOut])
def get_categories():
    return [MetaTagOut(key=k, label=v.get("label", k)) for k, v in load_categories().items()]


@router.get("/macro-tags", response_model=list[MetaTagOut])
def get_macro_tags():
    return [MetaTagOut(key=k, label=v.get("label", k)) for k, v in load_macro_tags().items()]
