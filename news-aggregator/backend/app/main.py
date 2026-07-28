import logging
from contextlib import asynccontextmanager
from pathlib import Path

from fastapi import FastAPI
from fastapi.staticfiles import StaticFiles

from app.db import init_db
from app.ingestion.scheduler import shutdown_scheduler, start_scheduler
from app.routers import ingest, items, meta, sources, translate

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(name)s: %(message)s")
logger = logging.getLogger("news_aggregator.main")

STATIC_DIR = Path(__file__).resolve().parent / "static"


@asynccontextmanager
async def lifespan(app: FastAPI):
    init_db()
    # The first ingestion run happens in the background (via the scheduler,
    # which fires immediately on start and then every POLL_INTERVAL_MINUTES)
    # rather than blocking here — the server must start accepting requests
    # right away, especially on hosts that spin the container down when
    # idle (e.g. Render's free tier), where every second before the first
    # response matters. The frontend also triggers its own ingestion run on
    # load, so the feed still populates within moments either way.
    start_scheduler()
    yield
    shutdown_scheduler()


app = FastAPI(title="Bilingual Signal-First News Aggregator", lifespan=lifespan)

app.include_router(items.router)
app.include_router(translate.router)
app.include_router(sources.router)
app.include_router(meta.router)
app.include_router(ingest.router)


@app.get("/api/health")
def health():
    return {"status": "ok"}


app.mount("/", StaticFiles(directory=str(STATIC_DIR), html=True), name="static")
