// Static server for a built ShinyLive site that emulates GitHub Pages delivery, so network-bound
// startup phases can be measured reproducibly regardless of machine load or the real network.
//
// Usage: node test_browser/throttled_server.js [dir=site] [port=8200] [kbytesPerSec=1400] [latencyMs=80]
//
// Emulated: one aggregate bandwidth budget shared by all connections, a fixed latency per request,
// `Cache-Control: max-age=600`, ETag + 304 revalidation, gzip for text/wasm types (not for the
// already-compressed .gz/.tgz/.data.gz files), and HEAD support (WebR probes metadata.rds with HEAD).
const http = require('http');
const fs = require('fs');
const path = require('path');
const zlib = require('zlib');
const crypto = require('crypto');

const ROOT = path.resolve(process.argv[2] || 'site');
const PORT = parseInt(process.argv[3] || '8200', 10);
const RATE = parseFloat(process.argv[4] || '1400') * 1024; // bytes per second, shared
const LATENCY = parseInt(process.argv[5] || '80', 10);
const CHUNK = 16 * 1024;

const TYPES = {
  '.html': 'text/html; charset=utf-8', '.js': 'text/javascript; charset=utf-8', '.mjs': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8', '.json': 'application/json; charset=utf-8', '.wasm': 'application/wasm',
  '.png': 'image/png', '.ico': 'image/x-icon', '.svg': 'image/svg+xml', '.woff2': 'font/woff2',
  '.gz': 'application/gzip', '.tgz': 'application/octet-stream', '.rds': 'application/octet-stream',
  '.txt': 'text/plain; charset=utf-8', '.map': 'application/json', '.metadata': 'application/octet-stream'
};
const COMPRESSIBLE = new Set(['.html', '.js', '.mjs', '.css', '.json', '.wasm', '.svg', '.txt', '.map', '.metadata']);

const gzipCache = new Map(); // file path -> { mtimeMs, buf }
let nextFree = Date.now(); // shared bandwidth scheduler
const stats = { requests: 0, bytes: 0, notModified: 0 };

function reserve(bytes) {
  const now = Date.now();
  const start = Math.max(now, nextFree);
  nextFree = start + (bytes / RATE) * 1000;
  return start - now;
}

async function sendThrottled(res, buf) {
  for (let off = 0; off < buf.length; off += CHUNK) {
    const part = buf.subarray(off, Math.min(off + CHUNK, buf.length));
    const wait = reserve(part.length);
    if (wait > 0) await new Promise((r) => setTimeout(r, wait));
    if (res.destroyed) return;
    if (!res.write(part)) await new Promise((r) => res.once('drain', r));
  }
  res.end();
}

http.createServer(async (req, res) => {
  try {
    stats.requests++;
    await new Promise((r) => setTimeout(r, LATENCY));
    let urlPath = decodeURIComponent(req.url.split('?')[0].split('#')[0]);
    if (urlPath.endsWith('/')) urlPath += 'index.html';
    const file = path.normalize(path.join(ROOT, urlPath));
    if (!file.startsWith(ROOT) || !fs.existsSync(file) || !fs.statSync(file).isFile()) {
      res.writeHead(404); return res.end('Not found');
    }
    const st = fs.statSync(file);
    const ext = path.extname(file).toLowerCase();
    const etag = '"' + crypto.createHash('md5').update(`${st.size}-${st.mtimeMs}`).digest('hex').slice(0, 16) + '"';
    const headers = {
      'Content-Type': TYPES[ext] || 'application/octet-stream',
      'Cache-Control': 'max-age=600', ETag: etag, 'Last-Modified': st.mtime.toUTCString(),
      'Accept-Ranges': 'none'
    };
    if (req.headers['if-none-match'] === etag) {
      stats.notModified++; res.writeHead(304, headers); return res.end();
    }
    let body = fs.readFileSync(file);
    if (COMPRESSIBLE.has(ext) && /\bgzip\b/.test(req.headers['accept-encoding'] || '')) {
      let c = gzipCache.get(file);
      if (!c || c.mtimeMs !== st.mtimeMs) { c = { mtimeMs: st.mtimeMs, buf: zlib.gzipSync(body, { level: 6 }) }; gzipCache.set(file, c); }
      body = c.buf; headers['Content-Encoding'] = 'gzip';
    }
    headers['Content-Length'] = body.length;
    res.writeHead(200, headers);
    stats.bytes += req.method === 'HEAD' ? 0 : body.length;
    if (req.method === 'HEAD') return res.end();
    await sendThrottled(res, body);
  } catch (err) {
    try { res.writeHead(500); res.end(String(err)); } catch (e) { /* connection already closed */ }
  }
}).listen(PORT, () => {
  console.log(`Throttled server: ${ROOT} on http://localhost:${PORT}  (${(RATE / 1024).toFixed(0)} KB/s shared, ${LATENCY} ms latency)`);
});

process.on('SIGINT', () => {
  console.log(`\nrequests=${stats.requests} bytesServed=${(stats.bytes / 1e6).toFixed(1)} MB notModified=${stats.notModified}`);
  process.exit(0);
});
