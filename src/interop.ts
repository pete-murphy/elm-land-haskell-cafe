import "@github/relative-time-element"
import "./main.css"

// This returns the flags passed into your Elm application
export const flags = async ({ env }: ElmLand.FlagsArgs) => {
  await ensureDatabaseInitialized()
  return {}
}

// This function is called after your Elm app starts
export const onReady = ({ app, env }: ElmLand.OnReadyArgs) => {
  console.log('Elm is ready', app)
  // Wire ports if present
  if (app.ports?.toBackend?.subscribe) {
    app.ports.toBackend.subscribe((msg: unknown) => {
      void handleInboundMessage(app, msg)
    })
  }
}


// Type definitions for Elm Land
namespace ElmLand {
  export type FlagsArgs = {
    env: Record<string, string>
  }
  export type OnReadyArgs = {
    env: Record<string, string>
    app: { ports?: Record<string, Port> }
  }
  export type Port = {
    send?: (data: unknown) => void
    subscribe?: (callback: (data: unknown) => unknown) => void
  }
}

class LocaleTime extends HTMLElement {
  static get observedAttributes() {
    return ['datetime']
  }
  options: Intl.DateTimeFormatOptions
  constructor() {
    super()
  }
  connectedCallback() {
    this.render()
  }
  attributeChangedCallback(name: string, oldValue: string, newValue: string) {
    this.render()
  }

  render() {
    const date = new Date(this.getAttribute('datetime')!)
    this.textContent = date.toLocaleString(undefined, this.options)
  }
}

customElements.define('locale-time', LocaleTime)

// -----------------------------------------------------------------------------
// PGlite initialization (persisted to IndexedDB)
// -----------------------------------------------------------------------------

let pgReadyPromise: Promise<void> | null = null
let pgInstance: import('@electric-sql/pglite').PGlite | null = null

type ElmApp = { ports?: Record<string, ElmLand.Port> }

type UpsertMessageItem = {
  id: string
  subject: string
  fromAddr: string
  dateIso: string
  inReplyTo: string | null
  references: string[] | null
  content: string
  monthFile: string
}

type Inbound =
  | { type: 'upsertMessages', file: string, index: number, items: UpsertMessageItem[] }
  | { type: 'getMessageById', id: string }
  | { type: 'getThread', rootId: string }
  | { type: 'searchMessages', query: string, limit?: number }

type Outbound =
  | { type: 'progress', file: string, index: number, inserted: number, total: number }
  | { type: 'done', file: string, index: number, insertedTotal: number }
  | { type: 'error', file: string, index: number, error: string }
  | { type: 'backpressure', level: number }
  | { type: 'message', data: any }
  | { type: 'thread', data: any[] }
  | { type: 'searchResults', data: any[] }

function sendToElm(app: ElmApp, message: Outbound) {
  const port = app.ports?.fromBackend
  if (port?.send) {
    port.send(message)
  } else {
    // Fallback logging if port not wired yet
    console.debug('fromBackend (no port):', message)
  }
}

async function ensureDatabaseInitialized(): Promise<void> {
  if (pgReadyPromise) return pgReadyPromise

  pgReadyPromise = (async () => {
    const { PGlite } = await import('@electric-sql/pglite')
    // Extensions: ltree + pg_trgm (loaded at init; schema created in next step)
    const { ltree } = await import('@electric-sql/pglite/contrib/ltree')
    const { pg_trgm } = await import('@electric-sql/pglite/contrib/pg_trgm')

    pgInstance = new PGlite('idb://haskell_cafe', {
      extensions: { ltree, pg_trgm }
    })
    // Touch the connection so initialization occurs deterministically
    await pgInstance.query('select 1')
    await ensureSchema()
  })()

  return pgReadyPromise
}

export function getPGlite(): import('@electric-sql/pglite').PGlite {
  if (!pgInstance) {
    throw new Error('PGlite not initialized yet. Call ensureDatabaseInitialized() first.')
  }
  return pgInstance
}

const EXPECTED_SCHEMA_VERSION = 2

async function handleInboundMessage(app: ElmApp, msg: unknown): Promise<void> {
  try {
    // Ensure PGlite is ready before handling any messages that touch the DB
    await ensureDatabaseInitialized()
    const t = (msg as any)?.type as string | undefined
    switch (t) {
      case 'upsertMessages': {
        const m = msg as Extract<Inbound, { type: 'upsertMessages' }>
        await enqueueUpsert(app, m.file, m.index, m.items)
        return
      }
      case 'getMessageById': {
        const m = msg as Extract<Inbound, { type: 'getMessageById' }>
        const row = await getMessageById(m.id)
        sendToElm(app, { type: 'message', data: row })
        return
      }
      case 'getThread': {
        const m = msg as Extract<Inbound, { type: 'getThread' }>
        const rows = await getThread(m.rootId)
        sendToElm(app, { type: 'thread', data: rows })
        return
      }
      case 'searchMessages': {
        const m = msg as Extract<Inbound, { type: 'searchMessages' }>
        const rows = await searchMessages(m.query, m.limit ?? 50)
        sendToElm(app, { type: 'searchResults', data: rows })
        return
      }
      default:
        // ignore
        return
    }
  } catch (e) {
    const error = e instanceof Error ? e.message : String(e)
    sendToElm(app, { type: 'error', file: 'unknown', index: -1, error })
  }
}

async function ensureSchema(): Promise<void> {
  const pg = getPGlite()
  // Ensure required extensions are available
  await pg.query(`CREATE EXTENSION IF NOT EXISTS ltree;`)
  await pg.query(`CREATE EXTENSION IF NOT EXISTS pg_trgm;`)
  // Create meta table if it doesn't exist
  await pg.query(`
    CREATE TABLE IF NOT EXISTS meta (
      schema_version INT NOT NULL,
      app_version TEXT,
      last_import_ts TIMESTAMP
    );
  `)
  // If meta row is missing, insert initial row
  const metaRes = await pg.query(`SELECT schema_version FROM meta LIMIT 1;`)
  if ((metaRes.rows as any[]).length === 0) {
    await pg.query(`INSERT INTO meta (schema_version, app_version, last_import_ts) VALUES ($1, $2, NULL);`, [EXPECTED_SCHEMA_VERSION, null])
  } else {
    const current = Number((metaRes.rows as any[])[0].schema_version)
    if (current !== EXPECTED_SCHEMA_VERSION) {
      // Rebuild strategy for now (source data can be re-imported)
      await pg.query('BEGIN;')
      await pg.query('DROP TABLE IF EXISTS messages;')
      await pg.query('COMMIT;')
      await pg.query('UPDATE meta SET schema_version = $1;', [EXPECTED_SCHEMA_VERSION])
    }
  }

  // Create messages table and indexes if not exist
  await pg.query(`
    CREATE TABLE IF NOT EXISTS messages (
      id TEXT PRIMARY KEY,
      subject TEXT,
      from_addr TEXT,
      date TIMESTAMP,
      in_reply_to TEXT,
      refs TEXT[],
      content TEXT,
      month_file TEXT,
      path LTREE NOT NULL,
      search TSVECTOR NOT NULL
    );
  `)
  // Indexes (create if not exists pattern via try/catch-less IF NOT EXISTS is supported for some index defs)
  await pg.query(`CREATE INDEX IF NOT EXISTS messages_path_gist ON messages USING GIST (path);`)
  await pg.query(`CREATE INDEX IF NOT EXISTS messages_search_gin ON messages USING GIN (search);`)
  await pg.query(`CREATE INDEX IF NOT EXISTS messages_date_idx ON messages (date);`)
  await pg.query(`CREATE INDEX IF NOT EXISTS messages_in_reply_to_idx ON messages (in_reply_to);`)
}

// -----------------------------------------------------------------------------
// Utilities
// -----------------------------------------------------------------------------

export function safe_ltree(label: string): string {
  // ltree labels allow A-Z a-z 0-9 _; map others to underscore, and collapse repeats
  const mapped = label.replace(/[^A-Za-z0-9_]/g, '_')
  const collapsed = mapped.replace(/_+/g, '_')
  // ltree labels cannot be empty; fallback to 'x'
  return collapsed.length > 0 ? collapsed : 'x'
}

export function computePath(parentPath: string | null, id: string): string {
  const label = safe_ltree(id)
  return parentPath && parentPath.length > 0 ? `${parentPath}.${label}` : label
}

// -----------------------------------------------------------------------------
// Debug queries (logged after each insert chunk)
// -----------------------------------------------------------------------------
async function logDebugQueries(): Promise<void> {
  const pg = getPGlite()
  try {
    const total = await pg.query(`SELECT COUNT(*)::int AS c FROM messages;`)
    const roots = await pg.query(`SELECT COUNT(*)::int AS c FROM messages WHERE in_reply_to IS NULL;`)
    const topSenders = await pg.query(`
      SELECT from_addr, COUNT(*)::int AS c
      FROM messages
      GROUP BY from_addr
      ORDER BY c DESC
      LIMIT 5;
    `)
    const largestThreads = await pg.query(`
      SELECT r.id AS root_id, COUNT(c.id)::int AS size
      FROM messages r
      JOIN messages c ON c.path <@ r.path
      WHERE r.in_reply_to IS NULL
      GROUP BY r.id
      ORDER BY size DESC
      LIMIT 5;
    `)
    const mostReplied = await pg.query(`
      SELECT p.id, p.subject, COUNT(c.id)::int AS replies
      FROM messages p
      JOIN messages c ON c.in_reply_to = p.id
      GROUP BY p.id, p.subject
      ORDER BY replies DESC
      LIMIT 5;
    `)
    const activeMonths = await pg.query(`
      SELECT month_file, COUNT(*)::int AS c
      FROM messages
      GROUP BY month_file
      ORDER BY c DESC
      LIMIT 5;
    `)
    const sampleSearch = await pg.query(`
      WITH q AS (SELECT plainto_tsquery('english', $1) AS tsq)
      SELECT m.id, m.subject
      FROM messages m, q
      WHERE m.search @@ q.tsq
      ORDER BY m.date DESC
      LIMIT 5;
    `, ['release'])
    console.log('[DB] Totals:', {
      total: (total.rows as any[])[0]?.c ?? 0,
      roots: (roots.rows as any[])[0]?.c ?? 0
    })
    console.log('[DB] Top senders:', topSenders.rows)
    console.log('[DB] Largest threads:', largestThreads.rows)
    console.log('[DB] Most replied-to:', mostReplied.rows)
    console.log('[DB] Most active months:', activeMonths.rows)
    console.log('[DB] Sample search "release":', sampleSearch.rows)
  } catch (e) {
    console.warn('[DB] debug queries failed:', e)
  }
}

// -----------------------------------------------------------------------------
// Single-writer queue and batched upsert
// -----------------------------------------------------------------------------

const writeQueue: Array<() => Promise<void>> = []
let processing = false
const BACKPRESSURE_THRESHOLD = 10

async function enqueue(job: () => Promise<void>, onBackpressure?: () => void): Promise<void> {
  writeQueue.push(job)
  if (writeQueue.length > BACKPRESSURE_THRESHOLD && onBackpressure) {
    onBackpressure()
  }
  if (!processing) {
    void processQueue()
  }
}

async function processQueue(): Promise<void> {
  processing = true
  try {
    while (writeQueue.length) {
      const job = writeQueue.shift()!
      await job()
    }
  } finally {
    processing = false
  }
}

async function enqueueUpsert(app: ElmApp, file: string, index: number, items: UpsertMessageItem[]): Promise<void> {
  const total = items.length
  let insertedTotal = 0
  const CHUNK_SIZE = 500
  const chunks: UpsertMessageItem[][] = []
  for (let i = 0; i < items.length; i += CHUNK_SIZE) {
    chunks.push(items.slice(i, i + CHUNK_SIZE))
  }

  const onBackpressure = () => sendToElm(app, { type: 'backpressure', level: writeQueue.length })

  for (const chunk of chunks) {
    await enqueue(async () => {
      const inserted = await upsertChunk(chunk)
      insertedTotal += inserted
      sendToElm(app, { type: 'progress', file, index, inserted: insertedTotal, total })
      if (insertedTotal >= total) {
        sendToElm(app, { type: 'done', file, index, insertedTotal })
      }
    }, onBackpressure)
  }
}

async function upsertChunk(chunk: UpsertMessageItem[]): Promise<number> {
  const pg = getPGlite()
  // Deduplicate by id within the chunk to avoid ON CONFLICT affecting the same row twice
  // If duplicates exist, keep the last occurrence (later data wins).
  const uniqueById = new Map<string, UpsertMessageItem>()
  const duplicatePairs: Array<{ id: string, first: UpsertMessageItem, second: UpsertMessageItem }> = []
  for (const item of chunk) {
    const prev = uniqueById.get(item.id)
    if (prev) {
      duplicatePairs.push({ id: item.id, first: prev, second: item })
    }
    uniqueById.set(item.id, item)
  }
  if (duplicatePairs.length > 0) {
    const sample = duplicatePairs.slice(0, 10).map(d => ({
      id: d.id,
      first: {
        subject: d.first.subject,
        fromAddr: d.first.fromAddr,
        dateIso: d.first.dateIso,
        inReplyTo: d.first.inReplyTo,
        monthFile: d.first.monthFile
      },
      second: {
        subject: d.second.subject,
        fromAddr: d.second.fromAddr,
        dateIso: d.second.dateIso,
        inReplyTo: d.second.inReplyTo,
        monthFile: d.second.monthFile
      }
    }))
    const ids = Array.from(new Set(duplicatePairs.map(d => d.id)))
    console.warn('[DB] Duplicate ids within upsert chunk', {
      count: duplicatePairs.length,
      uniqueIds: ids.length,
      ids: ids.slice(0, 25),
      sample
    })
  }
  const uniqueItems = Array.from(uniqueById.values())
  // Parent path lookup for items that have inReplyTo
  const parentIds = Array.from(new Set(uniqueItems.map(i => i.inReplyTo).filter((x): x is string => !!x)))
  let parentPathById = new Map<string, string>()
  if (parentIds.length > 0) {
    const params = parentIds.map((_, idx) => `$${idx + 1}`).join(', ')
    const res = await pg.query(`SELECT id, path FROM messages WHERE id IN (${params});`, parentIds)
    parentPathById = new Map((res.rows as any[]).map((r: any) => [r.id as string, r.path as string]))
  }

  // Prepare multirow insert with ON CONFLICT
  const values: any[] = []
  const rowsSql: string[] = []
  for (let i = 0; i < uniqueItems.length; i++) {
    const it = uniqueItems[i]
    const parentPath = it.inReplyTo ? (parentPathById.get(it.inReplyTo) ?? null) : null
    const path = computePath(parentPath, it.id)
    // to_tsvector computed in SQL from concatenated subject/content
    const baseIdx = i * 9
    rowsSql.push(`($${baseIdx + 1}, $${baseIdx + 2}, $${baseIdx + 3}, $${baseIdx + 4}, $${baseIdx + 5}, $${baseIdx + 6}, $${baseIdx + 7}, $${baseIdx + 8}, $${baseIdx + 9}, to_tsvector('english', coalesce($${baseIdx + 2}, '') || ' ' || coalesce($${baseIdx + 7}, '')))`)
    values.push(
      it.id,
      it.subject,
      it.fromAddr,
      new Date(it.dateIso).toISOString(),
      it.inReplyTo,
      it.references ?? null,
      it.content,
      it.monthFile,
      path
    )
  }

  const sql = `
    INSERT INTO messages (id, subject, from_addr, date, in_reply_to, refs, content, month_file, path, search)
    VALUES ${rowsSql.join(',\n')}
    ON CONFLICT (id) DO UPDATE SET
      subject = EXCLUDED.subject,
      from_addr = EXCLUDED.from_addr,
      date = EXCLUDED.date,
      in_reply_to = EXCLUDED.in_reply_to,
      refs = EXCLUDED.refs,
      content = EXCLUDED.content,
      month_file = EXCLUDED.month_file,
      path = EXCLUDED.path,
      search = EXCLUDED.search;
  `
  await pg.query(sql, values)
  // Log insights after each chunk upsert
  void logDebugQueries()
  return uniqueItems.length
}

// -----------------------------------------------------------------------------
// Read/query helpers
// -----------------------------------------------------------------------------

async function getMessageById(id: string): Promise<any | null> {
  const pg = getPGlite()
  const res = await pg.query(`SELECT * FROM messages WHERE id = $1 LIMIT 1;`, [id])
  return (res.rows as any[])[0] ?? null
}

async function getThread(rootId: string): Promise<any[]> {
  const pg = getPGlite()
  // Lookup root path, then fetch subtree
  const root = await pg.query(`SELECT path FROM messages WHERE id = $1 LIMIT 1;`, [rootId])
  if ((root.rows as any[]).length === 0) return []
  const rootPath = (root.rows as any[])[0].path as string
  const res = await pg.query(
    `SELECT * FROM messages WHERE path <@ $1 ORDER BY date;`,
    [rootPath]
  )
  return res.rows as any[]
}

async function searchMessages(query: string, limit: number): Promise<any[]> {
  const pg = getPGlite()
  // Combine FTS rank with recentness
  const res = await pg.query(
    `
    WITH q AS (SELECT plainto_tsquery('english', $1) AS tsq)
    SELECT m.*
    FROM messages m, q
    WHERE m.search @@ q.tsq
    ORDER BY ts_rank_cd(m.search, q.tsq) DESC, m.date DESC
    LIMIT $2;
    `,
    [query, limit]
  )
  return res.rows as any[]
}