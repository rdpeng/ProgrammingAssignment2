from fastapi import APIRouter, Depends
from sqlalchemy.orm import Session

from app.db import get_db
from app.ingestion.pipeline import run_ingestion
from app.schemas import IngestRunResult

router = APIRouter(prefix="/api/ingest", tags=["ingest"])


@router.post("/run", response_model=IngestRunResult)
def trigger_ingestion(db: Session = Depends(get_db)):
    """Run one ingestion pass synchronously and return stats.

    Bounded by the slowest feed's timeout (feeds are fetched concurrently),
    so this can take up to ~15-20s depending on network conditions.
    """
    stats = run_ingestion(db)
    return stats
