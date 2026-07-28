"""Must set env vars before any `app.*` module is imported, since app.db
creates the SQLAlchemy engine at import time from app.config.settings.
"""
import os
import tempfile
from pathlib import Path

_tmp_db_fd, _tmp_db_path = tempfile.mkstemp(suffix=".db")
os.close(_tmp_db_fd)
os.environ["DATABASE_URL"] = f"sqlite:///{_tmp_db_path}"
os.environ["TRANSLATION_PROVIDER"] = "none"

# Tests must never hit the network: point at an empty feed list so the
# FastAPI app's startup ingestion run (see app/main.py lifespan) is a no-op.
_empty_feeds_path = Path(tempfile.mkstemp(suffix=".yaml")[1])
_empty_feeds_path.write_text("sources: []\n")
os.environ["FEEDS_CONFIG_PATH"] = str(_empty_feeds_path)

import pytest  # noqa: E402

from app.db import Base, SessionLocal, engine  # noqa: E402


@pytest.fixture()
def db_session():
    Base.metadata.create_all(bind=engine)
    session = SessionLocal()
    try:
        yield session
    finally:
        session.close()
        Base.metadata.drop_all(bind=engine)
