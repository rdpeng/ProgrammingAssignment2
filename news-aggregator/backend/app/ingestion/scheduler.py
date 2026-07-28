import logging

from apscheduler.schedulers.background import BackgroundScheduler

from app.config import settings
from app.db import SessionLocal
from app.ingestion.pipeline import run_ingestion

logger = logging.getLogger("news_aggregator.scheduler")

_scheduler: BackgroundScheduler | None = None


def _run_ingestion_job():
    db = SessionLocal()
    try:
        stats = run_ingestion(db)
        logger.info("Ingestion run complete: %s", stats)
    except Exception:
        logger.exception("Scheduled ingestion run failed")
    finally:
        db.close()


def start_scheduler() -> BackgroundScheduler:
    global _scheduler
    if _scheduler is not None:
        return _scheduler
    _scheduler = BackgroundScheduler(timezone="UTC")
    _scheduler.add_job(
        _run_ingestion_job,
        "interval",
        minutes=settings.poll_interval_minutes,
        id="poll_feeds",
        # No next_run_time override -> APScheduler fires the first run
        # immediately (in this background thread), then every interval
        # after. Runs in the background so it never blocks the server from
        # accepting requests — important on hosts like Render's free tier,
        # where the whole container is cold and every second before the
        # server responds matters.
    )
    _scheduler.start()
    return _scheduler


def shutdown_scheduler():
    global _scheduler
    if _scheduler is not None:
        _scheduler.shutdown(wait=False)
        _scheduler = None
