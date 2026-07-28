# Signal — Bilingual (EN/HI) Signal-First News Aggregator

A free-to-run news aggregator for a financially literate Indian reader who
wants to know what's happening *and why it matters to markets*. It pulls
from public RSS/Atom feeds (no paid news API required), deduplicates
cross-outlet coverage into single stories, tags each story by category and
by **macro-impact** (crude, INR, rates, metals, trade, supply-chain,
geopolitics, agriculture, energy), and serves a fast bilingual feed with
on-demand EN⇄HI translation.

Signal over volume: when in doubt, the pipeline shows fewer high-signal
items rather than a firehose, and it never fabricates a summary or a
source — if a feed gives nothing useful, you get the raw headline and a
link.

## Get it on your iPhone (no coding required)

1. **Deploy it to Render** (a free hosting service) so it's reachable from
   anywhere, not just your own computer:
   - Go to [render.com](https://render.com) and sign up (free, "Sign up with
     GitHub" is easiest).
   - Click **New +** → **Blueprint**.
   - Pick this GitHub repository. Render will find the `render.yaml` at the
     repo root and pre-fill everything (Dockerfile path, health check, free
     plan). Click **Apply** / **Create**.
   - Wait for the first build to finish (a few minutes). Render gives you a
     URL like `https://signal-news-xxxx.onrender.com`.
2. **Open that URL on your iPhone** in Safari (not Chrome — "Add to Home
   Screen" as a full app only works from Safari).
3. Tap the **Share** icon (square with an arrow) → **Add to Home Screen** →
   **Add**. You now have a "Signal" app icon on your home screen that opens
   full-screen, no browser bar.

**Free-tier tradeoff, honestly:** Render's free plan puts the app to sleep
after 15 minutes with no visitors, and — because the free plan has no
persistent storage — every time it wakes up it starts with an empty feed
and re-fetches everything from scratch (roughly 30–60 seconds total: wake-up
+ re-fetch). Since it auto-refreshes every time you open the app anyway,
this mostly means: if you haven't opened it in a while, the first open of
the day is a bit slow, then it's instant for the rest of the day as you
keep reopening it. If that's annoying, Render's paid Starter plan (~$7/mo)
keeps it always-on with a persistent database — see
[Render's pricing](https://render.com/pricing) if you want that later; no
code changes needed, just a plan upgrade in Render's dashboard.

## Copyright & usage note

This app stores and displays **only**: headline, source name, publish
time, canonical link, and a short (≤40-word) original summary derived
from the publisher's own RSS teaser text. It never stores or displays full
article bodies, and every card links out to the original source. Optional
LLM-based summarization (see below) rewrites that same short teaser into a
tighter original sentence — it does not read or summarize full articles.

## Quickstart (Docker)

```bash
cp backend/.env.example backend/.env   # optional — see "Environment variables" below
docker compose up --build
```

Open http://localhost:8000. The server starts accepting requests
immediately; a first poll of all enabled feeds kicks off in the background
right away (so the feed fills in within moments, not on a blocking delay),
then repeats every `POLL_INTERVAL_MINUTES` (default 15). The frontend also
triggers a poll whenever you open the app. You can always hit **Refresh**
in the UI, or `POST /api/ingest/run`, to poll immediately.

## Quickstart (local, no Docker)

Requires Python 3.11+.

```bash
cd backend
python3 -m venv .venv && source .venv/bin/activate
pip install -r requirements.txt
python scripts/ingest_once.py     # seed the DB with one ingestion pass
uvicorn app.main:app --reload     # http://localhost:8000
```

`scripts/ingest_once.py` is also handy for cron-based deployments as an
alternative to the built-in in-process scheduler.

## Adding / editing feeds

Edit `backend/config/feeds.yaml` — no code changes needed. Each entry:

```yaml
- name: The Hindu - National
  feed_url: "https://www.thehindu.com/news/national/feeder/default.rss"
  language: en        # "en" or "hi"
  category: india_politics_policy
  enabled: true
```

A feed that's unreachable or changes its RSS path is logged and skipped —
it never breaks the rest of the ingestion run. Outlets change RSS URLs
periodically; if a source stops producing new items, check the app logs
for `Failed to fetch feed ...` warnings and update its `feed_url` here.

Categories and macro-impact tags are keyword-rule files, also editable
without touching code:

- `backend/config/categories.yaml` — category → keyword list
- `backend/config/macro_tags.yaml` — macro tag → keyword list (the
  "what actually moves markets" layer — crude, metals, rates, INR, FX,
  trade, supply-chain, geopolitics, agriculture, energy)

Both support multiple matches per item, and edits take effect on the next
request (the config loader watches file mtimes — no restart needed).

## Environment variables

Everything is optional; the app runs on RSS + SQLite + no keys by
default. See `backend/.env.example` for the full list, including:

- **Translation** (`TRANSLATION_PROVIDER`): defaults to `google_free`
  (deep-translator's keyless endpoint, best-effort). Set to
  `libretranslate` + `LIBRETRANSLATE_URL` for a self-hosted instance, or
  `none` to disable. Translation is always on-demand (per-card toggle in
  the UI) and never blocks ingestion — if it's unavailable, the original-
  language text is shown with an inline note.
- **LLM summarizer** (`LLM_SUMMARIZER_PROVIDER=anthropic` +
  `ANTHROPIC_API_KEY`): optional upgrade over the default extractive
  summary (which truncates the publisher's own RSS teaser to ≤40 words).
  Falls back to extractive automatically if unset or if the call fails.
- **Optional free-tier news APIs** (`GNEWS_API_KEY`, `NEWSDATA_API_KEY`,
  `CURRENTS_API_KEY`): each is a no-op unless its key is set, and any
  failure is logged and skipped — RSS ingestion is never blocked by them.
- **Dedup thresholds** (`DEDUP_TITLE_SIMILARITY_THRESHOLD`,
  `DEDUP_COMBINED_SIMILARITY_THRESHOLD`): tune if you find stories
  wrongly merged or wrongly left separate — see
  `backend/app/processing/dedup.py` for how they're used.

## How it works

```
feeds.yaml ──▶ fetch (httpx, concurrent) ──▶ normalize (feedparser)
                                                    │
                                                    ▼
                              canonicalize URL + fuzzy title/summary match
                                        (rapidfuzz, processing/dedup.py)
                                                    │
                          ┌─────────────────────────┴───────────────────┐
                          ▼                                             ▼
                new cluster (new story)                    join existing cluster
                                                                  ("N sources")
                                                    │
                                                    ▼
                          keyword-rule tagging (category + macro-impact)
                                                    │
                                                    ▼
                              extractive (or optional LLM) summary, ≤40 words
                                                    │
                                                    ▼
                                          SQLite: sources / items / tags /
                                          clusters / cluster_items
                                                    │
                                                    ▼
                                    FastAPI JSON API ──▶ static bilingual feed UI
```

Data model (SQLAlchemy, see `backend/app/models.py`):
`sources(id, name, feed_url, language, category, enabled)`,
`items(id, source_id, title, url, url_canonical, summary, language,
published_at, fetched_at, translated_*)`,
`tags(item_id, tag, tag_type)`,
`clusters(id, representative_item_id)`,
`cluster_items(cluster_id, item_id)`.

Dedup is a heuristic, not perfect: it combines title-only similarity (for
near-identical/wire-syndicated headlines) with title+summary similarity
(for paraphrased cross-outlet coverage, since headlines alone vary a lot
between outlets — "hikes" vs "raises", "bps" vs "basis points"). Tune the
two thresholds in `.env` if you see it over- or under-merging.

## API

- `GET /api/items?category=&macro=&language=&source_id=&since_hours=&q=&limit=&offset=`
  — one card per story (cluster), newest first
- `GET /api/items/{cluster_id}/sources` — all outlets covering that story
- `POST /api/items/{item_id}/translate?target=en|hi` — on-demand translate,
  cached after first call
- `POST /api/items/{item_id}/tags` — manual re-tag (`{"tag": "...",
  "tag_type": "topic", "action": "add"|"remove"}`)
- `GET /api/sources`, `GET /api/meta/categories`, `GET /api/meta/macro-tags`
- `POST /api/ingest/run` — trigger one ingestion pass now

Bookmarks and "priority topic" pinning live in browser `localStorage`
(no account/auth in this single-user build).

## Testing

```bash
cd backend
pip install -r requirements-dev.txt
python -m pytest tests/ -q
```

Covers tagging rules, summarization word-capping, URL canonicalization,
dedup clustering (paraphrased cross-outlet merge, unrelated stories
staying separate, duplicate-URL handling), and the API layer.

## Deploying for free

`docker compose up` works on any VPS with a free tier (e.g. Oracle Cloud's
always-free tier, a low-end Hetzner/DigitalOcean box, or Fly.io's free
allowance) — put a reverse proxy (Caddy/Nginx) with a free Let's Encrypt
cert in front of port 8000. SQLite is fine at this scale (single reader
UI, a few thousand items); the `news_data` Docker volume persists it
across restarts.
