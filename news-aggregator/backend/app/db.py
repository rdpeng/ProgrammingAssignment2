from sqlalchemy import create_engine
from sqlalchemy.orm import DeclarativeBase, sessionmaker

from app.config import settings

db_path = settings.database_url
if db_path.startswith("sqlite:///"):
    file_path = db_path.replace("sqlite:///", "", 1)
    from pathlib import Path

    Path(file_path).parent.mkdir(parents=True, exist_ok=True)

connect_args = {"check_same_thread": False} if db_path.startswith("sqlite") else {}
engine = create_engine(db_path, connect_args=connect_args)
SessionLocal = sessionmaker(autocommit=False, autoflush=False, bind=engine)


class Base(DeclarativeBase):
    pass


def get_db():
    db = SessionLocal()
    try:
        yield db
    finally:
        db.close()


def init_db():
    from app import models  # noqa: F401  (ensure models are registered)

    Base.metadata.create_all(bind=engine)
