FROM python:3.11-slim

WORKDIR /app

RUN apt-get update && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY . .

RUN mkdir -p /app/data
VOLUME ["/app/data"]

EXPOSE 8000

# $PORT is injected by most free hosts (Render, Railway, etc.); default to
# 8000 for plain `docker run` / docker-compose where nothing sets it.
ENV PORT=8000

HEALTHCHECK --interval=30s --timeout=5s --start-period=15s \
  CMD curl -f "http://localhost:${PORT}/api/health" || exit 1

CMD ["sh", "-c", "uvicorn app.main:app --host 0.0.0.0 --port ${PORT}"]
