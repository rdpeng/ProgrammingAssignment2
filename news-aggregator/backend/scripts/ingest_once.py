#!/usr/bin/env python3
"""Run a single ingestion pass synchronously. Useful for first-run seeding,
cron-based deployments (instead of the in-process scheduler), and testing.

Usage: python scripts/ingest_once.py
"""
import logging
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from app.db import SessionLocal, init_db  # noqa: E402
from app.ingestion.pipeline import run_ingestion  # noqa: E402

logging.basicConfig(level=logging.INFO, format="%(asctime)s %(levelname)s %(name)s: %(message)s")


def main():
    init_db()
    db = SessionLocal()
    try:
        stats = run_ingestion(db)
        print("Ingestion complete:")
        for key, value in stats.items():
            print(f"  {key}: {value}")
    finally:
        db.close()


if __name__ == "__main__":
    main()
