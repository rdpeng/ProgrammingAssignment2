from fastapi import APIRouter, Depends, HTTPException, Query
from sqlalchemy.orm import Session

from app.config import settings
from app.db import get_db
from app.models import Item
from app.processing.translate import translate_item
from app.schemas import TranslateResponse

router = APIRouter(prefix="/api/items", tags=["translate"])


@router.post("/{item_id}/translate", response_model=TranslateResponse)
def translate(
    item_id: int,
    target: str = Query(..., pattern="^(en|hi)$"),
    db: Session = Depends(get_db),
):
    item = db.get(Item, item_id)
    if item is None:
        raise HTTPException(status_code=404, detail="Item not found")

    if item.language == target:
        raise HTTPException(status_code=400, detail="Item is already in the target language")

    if item.translated_language == target and item.translated_title:
        return TranslateResponse(
            item_id=item.id,
            translated_title=item.translated_title,
            translated_summary=item.translated_summary or "",
            translated_language=target,
            provider=settings.translation_provider,
        )

    result = translate_item(item.title, item.summary, item.language, target)
    if result is None:
        raise HTTPException(
            status_code=503,
            detail="Translation unavailable right now (provider unreachable or disabled). "
            "Original-language text is still shown.",
        )

    t_title, t_summary = result
    item.translated_title = t_title
    item.translated_summary = t_summary
    item.translated_language = target
    db.commit()

    return TranslateResponse(
        item_id=item.id,
        translated_title=t_title,
        translated_summary=t_summary,
        translated_language=target,
        provider=settings.translation_provider,
    )
