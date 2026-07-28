// Signal — bilingual news feed frontend. No build step: vanilla JS + fetch.
'use strict';

const API = '';
const LS_BOOKMARKS = 'signal_bookmarks_v1';
const LS_PRIORITY = 'signal_priority_v1';
const LS_THEME = 'signal_theme_v1';

// ---------- tiny persisted-state helpers ----------
function loadSet(key) {
  try { return new Set(JSON.parse(localStorage.getItem(key) || '[]')); }
  catch { return new Set(); }
}
function saveSet(key, set) { localStorage.setItem(key, JSON.stringify([...set])); }

const bookmarks = loadSet(LS_BOOKMARKS);          // Set<item_id>
const priorityTags = loadSet(LS_PRIORITY);        // Set<"category:key" | "macro:key">

// ---------- state ----------
const state = {
  category: new Set(),
  macro: new Set(),
  language: '',
  sinceHours: 24,
  q: '',
  bookmarkedOnly: false,
  offset: 0,
  limit: 30,
  items: [],       // accumulated feed cards
  clusterCache: {},// cluster_id -> members (lazy loaded)
  translateUi: {}, // item_id -> { loading, error, show }
};

let categoriesMeta = [];
let macroMeta = [];

// ---------- utils ----------
function parseUTC(iso) {
  // Backend serializes naive UTC datetimes (no offset suffix); make that explicit.
  return new Date(iso.endsWith('Z') || /[+-]\d\d:\d\d$/.test(iso) ? iso : iso + 'Z');
}

function relativeTime(iso) {
  const d = parseUTC(iso);
  const diffMs = Date.now() - d.getTime();
  const mins = Math.round(diffMs / 60000);
  if (mins < 1) return 'just now';
  if (mins < 60) return `${mins}m ago`;
  const hrs = Math.round(mins / 60);
  if (hrs < 24) return `${hrs}h ago`;
  const days = Math.round(hrs / 24);
  if (days < 7) return `${days}d ago`;
  return d.toLocaleDateString(undefined, { month: 'short', day: 'numeric' });
}

function escapeHtml(s) {
  return (s || '').replace(/[&<>"']/g, (c) => ({
    '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;',
  }[c]));
}

function debounce(fn, ms) {
  let t;
  return (...args) => { clearTimeout(t); t = setTimeout(() => fn(...args), ms); };
}

async function api(path, opts) {
  const resp = await fetch(API + path, opts);
  if (!resp.ok) {
    const detail = await resp.json().catch(() => ({}));
    const err = new Error(detail.detail || `HTTP ${resp.status}`);
    err.status = resp.status;
    throw err;
  }
  return resp.status === 204 ? null : resp.json();
}

function setStatus(msg, isError) {
  const el = document.getElementById('statusBar');
  el.textContent = msg || '';
  el.classList.toggle('error', !!isError);
}

// ---------- theme ----------
function applyTheme() {
  const saved = localStorage.getItem(LS_THEME);
  const dark = saved ? saved === 'dark' : window.matchMedia('(prefers-color-scheme: dark)').matches;
  document.documentElement.classList.toggle('dark', dark);
  document.getElementById('darkIcon').innerHTML = dark
    ? '<circle cx="12" cy="12" r="4"/><path d="M12 2v2M12 20v2M4.9 4.9l1.4 1.4M17.7 17.7l1.4 1.4M2 12h2M20 12h2M4.9 19.1l1.4-1.4M17.7 6.3l1.4-1.4"/>'
    : '<path d="M21 12.8A9 9 0 1 1 11.2 3 7 7 0 0 0 21 12.8Z"/>';
}
document.getElementById('darkToggle').addEventListener('click', () => {
  const nowDark = !document.documentElement.classList.contains('dark');
  localStorage.setItem(LS_THEME, nowDark ? 'dark' : 'light');
  applyTheme();
});
applyTheme();

// ---------- meta / filter chips ----------
async function loadMeta() {
  [categoriesMeta, macroMeta] = await Promise.all([
    api('/api/meta/categories'),
    api('/api/meta/macro-tags'),
  ]);
  renderCategoryChips();
  renderMacroChips();
  renderPriorityChips();
}

function chipClasses(active, tone) {
  return 'chip' + (tone === 'macro' ? ' macro' : '') + (active ? ' active' : '');
}

function renderCategoryChips() {
  const el = document.getElementById('categoryChips');
  el.innerHTML = categoriesMeta.map(c => `
    <button class="${chipClasses(state.category.has(c.key), 'category')}" data-cat="${c.key}">${escapeHtml(c.label)}</button>
  `).join('');
}

function renderMacroChips() {
  const el = document.getElementById('macroChips');
  el.innerHTML = macroMeta.map(m => `
    <button class="${chipClasses(state.macro.has(m.key), 'macro')}" data-macro="${m.key}">${escapeHtml(m.label)}</button>
  `).join('');
}

function renderPriorityChips() {
  const el = document.getElementById('priorityChips');
  const all = [
    ...categoriesMeta.map(c => ({ type: 'category', key: c.key, label: c.label })),
    ...macroMeta.map(m => ({ type: 'macro', key: m.key, label: m.label })),
  ];
  el.innerHTML = all.map(t => {
    const id = `${t.type}:${t.key}`;
    const active = priorityTags.has(id);
    return `<button class="${chipClasses(active, t.type)}" data-priority="${id}">${active ? '📌 ' : ''}${escapeHtml(t.label)}</button>`;
  }).join('');
}

document.getElementById('categoryChips').addEventListener('click', (e) => {
  const btn = e.target.closest('[data-cat]');
  if (!btn) return;
  const key = btn.dataset.cat;
  state.category.has(key) ? state.category.delete(key) : state.category.add(key);
  renderCategoryChips();
  resetAndLoad();
});

document.getElementById('macroChips').addEventListener('click', (e) => {
  const btn = e.target.closest('[data-macro]');
  if (!btn) return;
  const key = btn.dataset.macro;
  state.macro.has(key) ? state.macro.delete(key) : state.macro.add(key);
  renderMacroChips();
  resetAndLoad();
});

document.getElementById('priorityChips').addEventListener('click', (e) => {
  const btn = e.target.closest('[data-priority]');
  if (!btn) return;
  const id = btn.dataset.priority;
  priorityTags.has(id) ? priorityTags.delete(id) : priorityTags.add(id);
  saveSet(LS_PRIORITY, priorityTags);
  renderPriorityChips();
  renderFeed(); // pure re-sort, no refetch
});

document.getElementById('priorityHelpBtn').addEventListener('click', () => {
  document.getElementById('priorityPanel').classList.toggle('hidden');
});

function syncLangChips() {
  document.querySelectorAll('.lang-chip').forEach(b =>
    b.classList.toggle('active', b.dataset.lang === state.language));
}
document.querySelectorAll('.lang-chip').forEach(btn => {
  btn.addEventListener('click', () => {
    state.language = btn.dataset.lang;
    syncLangChips();
    resetAndLoad();
  });
});
syncLangChips(); // "All" active by default

document.getElementById('windowSelect').addEventListener('change', (e) => {
  state.sinceHours = e.target.value;
  resetAndLoad();
});

document.getElementById('searchInput').addEventListener('input', debounce((e) => {
  state.q = e.target.value.trim();
  resetAndLoad();
}, 350));

document.getElementById('bookmarkedOnly').addEventListener('change', (e) => {
  state.bookmarkedOnly = e.target.checked;
  resetAndLoad();
});

// ---------- ingestion trigger ----------
// Runs on: manual Refresh tap, every time the app is opened/foregrounded
// (cold load or returning from the background), and every 15 min in the
// background on the server regardless (see POLL_INTERVAL_MINUTES).
let lastIngestAt = 0;
let ingestInFlight = false;

async function runIngest({ silent } = {}) {
  if (ingestInFlight) return;
  ingestInFlight = true;
  const icon = document.getElementById('refreshIcon');
  const btn = document.getElementById('refreshBtn');
  btn.disabled = true;
  icon.classList.add('spin');
  if (!silent) setStatus('Polling feeds for the latest stories…');
  try {
    const stats = await api('/api/ingest/run', { method: 'POST' });
    lastIngestAt = Date.now();
    setStatus(`Fetched ${stats.items_new} new items from ${stats.sources_polled - stats.sources_failed}/${stats.sources_polled} sources` +
      (stats.sources_failed ? ` (${stats.sources_failed} feed(s) unreachable)` : ''));
    await resetAndLoad();
  } catch (err) {
    if (!silent) setStatus('Refresh failed: ' + err.message, true);
  } finally {
    btn.disabled = false;
    icon.classList.remove('spin');
    ingestInFlight = false;
  }
}

document.getElementById('refreshBtn').addEventListener('click', () => runIngest());

// Opening the app (cold start, or bringing it back to the foreground from
// the iPhone home screen / app switcher) counts as "manually opening" —
// refresh right away, but don't hammer the server on rapid app-switching.
const REOPEN_REFRESH_COOLDOWN_MS = 60 * 1000;
document.addEventListener('visibilitychange', () => {
  if (document.visibilityState === 'visible' && Date.now() - lastIngestAt > REOPEN_REFRESH_COOLDOWN_MS) {
    runIngest({ silent: true });
  }
});

// ---------- feed fetching ----------
function buildQuery() {
  const p = new URLSearchParams();
  state.category.forEach(c => p.append('category', c));
  state.macro.forEach(m => p.append('macro', m));
  if (state.language) p.set('language', state.language);
  if (state.sinceHours) p.set('since_hours', state.sinceHours);
  if (state.q) p.set('q', state.q);
  p.set('limit', state.bookmarkedOnly ? 200 : state.limit);
  p.set('offset', state.bookmarkedOnly ? 0 : state.offset);
  return p.toString();
}

async function resetAndLoad() {
  state.offset = 0;
  state.items = [];
  await loadMore(true);
}

async function loadMore(replace) {
  setStatus('Loading…');
  try {
    const batch = await api('/api/items?' + buildQuery());
    state.items = replace ? batch : state.items.concat(batch);
    state.offset += batch.length;
    document.getElementById('loadMoreBtn').classList.toggle('hidden', state.bookmarkedOnly || batch.length < state.limit);
    setStatus(state.items.length ? `${state.items.length} stories` : '');
    renderFeed();
  } catch (err) {
    setStatus('Could not load feed: ' + err.message, true);
  }
}

document.getElementById('loadMoreBtn').addEventListener('click', () => loadMore(false));

// ---------- rendering ----------
function isPinned(card) {
  return card.tags.some(t => priorityTags.has(`${t.tag_type}:${t.tag}`));
}

function sortedItems() {
  let items = state.items;
  if (state.bookmarkedOnly) items = items.filter(c => bookmarks.has(c.item_id));
  if (priorityTags.size === 0) return items;
  return [...items].sort((a, b) => (isPinned(b) ? 1 : 0) - (isPinned(a) ? 1 : 0));
}

function tagChip(tag) {
  const meta = tag.tag_type === 'macro' ? macroMeta : categoriesMeta;
  const label = meta.find(m => m.key === tag.tag)?.label || tag.tag;
  const cls = tag.tag_type === 'macro' ? 'tag-chip macro' : 'tag-chip';
  return `<span class="${cls}">${tag.tag_type === 'macro' ? '⚡ ' : ''}${escapeHtml(label)}</span>`;
}

function cardHtml(card) {
  const ui = state.translateUi[card.item_id] || {};
  const pinned = isPinned(card);
  const isBookmarked = bookmarks.has(card.item_id);
  const showingTranslated = ui.show && card.translated_title;
  const title = showingTranslated ? card.translated_title : card.title;
  const summary = showingTranslated ? card.translated_summary : card.summary;
  const translateLabel = card.language === 'hi' ? 'EN' : 'हिं';

  return `
  <article class="card${pinned ? ' pinned' : ''}" data-item-id="${card.item_id}" data-cluster-id="${card.cluster_id}">
    <div class="card-top">
      <div class="card-body">
        <div class="card-meta">
          ${pinned ? '<span title="Pinned priority topic">📌</span>' : ''}
          <span class="source-name">${escapeHtml(card.source_name)}</span>
          <span>·</span>
          <span>${relativeTime(card.published_at)}</span>
          <span class="lang-badge">${card.language}</span>
        </div>
        <a href="${escapeHtml(card.url)}" target="_blank" rel="noopener noreferrer" class="card-title">
          ${escapeHtml(title)}
        </a>
        <p class="card-summary">${escapeHtml(summary)}</p>
        ${ui.error ? `<p class="card-error">${escapeHtml(ui.error)}</p>` : ''}
      </div>
      <div class="card-actions">
        <button class="bookmark-btn" title="${isBookmarked ? 'Remove bookmark' : 'Read later'}">
          ${isBookmarked ? '★' : '☆'}
        </button>
        <button class="translate-btn">
          ${ui.loading ? '…' : (showingTranslated ? 'original' : translateLabel)}
        </button>
      </div>
    </div>
    <div class="tag-row">
      ${card.tags.map(tagChip).join('')}
      ${card.source_count > 1 ? `<button class="sources-btn">${card.source_count} sources ▾</button>` : ''}
    </div>
    <div class="sources-list hidden"></div>
  </article>`;
}

function renderFeed() {
  const items = sortedItems();
  document.getElementById('feed').innerHTML = items.map(cardHtml).join('');
  document.getElementById('emptyState').classList.toggle('hidden', items.length > 0);
}

// ---------- feed event delegation ----------
document.getElementById('feed').addEventListener('click', async (e) => {
  const article = e.target.closest('article');
  if (!article) return;
  const itemId = Number(article.dataset.itemId);
  const clusterId = Number(article.dataset.clusterId);
  const card = state.items.find(c => c.item_id === itemId);

  if (e.target.closest('.bookmark-btn')) {
    bookmarks.has(itemId) ? bookmarks.delete(itemId) : bookmarks.add(itemId);
    saveSet(LS_BOOKMARKS, bookmarks);
    if (state.bookmarkedOnly) renderFeed();
    else {
      const btn = article.querySelector('.bookmark-btn');
      const nowMarked = bookmarks.has(itemId);
      btn.textContent = nowMarked ? '★' : '☆';
      btn.title = nowMarked ? 'Remove bookmark' : 'Read later';
    }
    return;
  }

  if (e.target.closest('.translate-btn')) {
    const ui = state.translateUi[itemId] || {};
    if (ui.show) { // toggle back to original, no re-fetch
      state.translateUi[itemId] = { ...ui, show: false };
      renderFeed();
      return;
    }
    if (card.translated_title) {
      state.translateUi[itemId] = { show: true };
      renderFeed();
      return;
    }
    state.translateUi[itemId] = { loading: true };
    renderFeed();
    const target = card.language === 'hi' ? 'en' : 'hi';
    try {
      const result = await api(`/api/items/${itemId}/translate?target=${target}`, { method: 'POST' });
      card.translated_title = result.translated_title;
      card.translated_summary = result.translated_summary;
      state.translateUi[itemId] = { show: true };
    } catch (err) {
      state.translateUi[itemId] = { error: err.message || 'Translation unavailable' };
    }
    renderFeed();
    return;
  }

  if (e.target.closest('.sources-btn')) {
    const list = article.querySelector('.sources-list');
    const willShow = list.classList.contains('hidden');
    if (willShow && !state.clusterCache[clusterId]) {
      list.innerHTML = '<p class="muted">Loading…</p>';
      list.classList.remove('hidden');
      try {
        state.clusterCache[clusterId] = await api(`/api/items/${clusterId}/sources`);
      } catch {
        list.innerHTML = '<p class="card-error">Could not load sources.</p>';
        return;
      }
    }
    if (willShow) {
      const members = state.clusterCache[clusterId] || [];
      list.innerHTML = members.map(m => `
        <div>
          <a href="${escapeHtml(m.url)}" target="_blank" rel="noopener noreferrer">${escapeHtml(m.title)}</a>
          <span class="muted"> — ${escapeHtml(m.source_name)}, ${relativeTime(m.published_at)}</span>
        </div>`).join('');
    }
    list.classList.toggle('hidden');
    return;
  }
});

// ---------- boot ----------
(async function init() {
  try {
    await loadMeta();
    await resetAndLoad(); // show whatever's already cached, instantly
    runIngest({ silent: true }); // then check for anything new, in the background
  } catch (err) {
    setStatus('Failed to load app: ' + err.message, true);
  }
})();
