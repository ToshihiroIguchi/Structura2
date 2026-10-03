const puppeteer = require('puppeteer-core');
const fs = require('fs');
const path = require('path');

async function runDetailedProfile() {
  console.log('================================================================');
  console.log('       STRUCTURA2 DETAILED STARTUP PROFILING & BENCHMARK        ');
  console.log('================================================================');

  // Launch browser for Cold Start
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });
  await page.setCacheEnabled(false);

  const t0 = Date.now();
  const timeline = [];
  const addTimeline = (source, label, meta = {}) => {
    const ms = Date.now() - t0;
    timeline.push({ ms, sec: +(ms / 1000).toFixed(3), source, label, meta });
    console.log(`[${(ms / 1000).toFixed(3).padStart(6, ' ')}s] [${source}] ${label}`);
  };

  const requests = new Map();

  page.on('console', msg => {
    const text = msg.text();
    addTimeline('CONSOLE', text.substring(0, 140));
  });

  page.on('pageerror', err => {
    addTimeline('PAGE_ERROR', err.message);
  });

  page.on('request', req => {
    const url = req.url();
    requests.set(url, {
      url,
      method: req.method(),
      resourceType: req.resourceType(),
      startMs: Date.now() - t0,
      endMs: null,
      durationMs: null,
      size: 0
    });
  });

  page.on('requestfinished', req => {
    const url = req.url();
    const item = requests.get(url);
    if (item) {
      item.endMs = Date.now() - t0;
      item.durationMs = item.endMs - item.startMs;
      const resp = req.response();
      if (resp) {
        const headers = resp.headers();
        item.size = parseInt(headers['content-length'] || '0', 10);
      }
      const shortUrl = url.replace('http://localhost:8100', '');
      if (item.size > 200000 || item.durationMs > 200 || url.includes('.wasm') || url.includes('app.json') || url.includes('.tgz') || url.includes('.data.gz')) {
        addTimeline('NETWORK', `Done: ${shortUrl.substring(0, 60)} (${item.durationMs}ms, ${(item.size / 1024).toFixed(1)} KB)`);
      }
    }
  });

  page.on('requestfailed', req => {
    const url = req.url();
    const item = requests.get(url);
    if (item) {
      item.endMs = Date.now() - t0;
      item.durationMs = item.endMs - item.startMs;
      item.failed = true;
    }
    addTimeline('NET_FAIL', url);
  });

  addTimeline('LIFECYCLE', 'Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'load', timeout: 120000 });
  addTimeline('LIFECYCLE', 'Outer page load event fired.');

  addTimeline('LIFECYCLE', 'Waiting for Shinylive iframe element...');
  const iframeElem = await page.waitForSelector('iframe', { timeout: 30000 });
  const frame = await iframeElem.contentFrame();
  addTimeline('LIFECYCLE', 'Shinylive iframe attached.');

  addTimeline('LIFECYCLE', 'Waiting for Shiny UI (#sample_ds modal)...');
  await frame.waitForSelector('#sample_ds', { timeout: 120000 });
  const ttiCold = Date.now() - t0;
  addTimeline('LIFECYCLE', `=== APP READY (Cold Start TTI: ${(ttiCold / 1000).toFixed(3)}s) ===`);

  // Grab performance timings from outer page
  const outerPerf = await page.evaluate(() => {
    const nav = performance.getEntriesByType('navigation')[0];
    return nav ? nav.toJSON() : null;
  });

  // Grab performance timings from iframe
  const iframePerf = await frame.evaluate(() => {
    const nav = performance.getEntriesByType('navigation')[0];
    return nav ? nav.toJSON() : null;
  });

  await browser.close();

  // Now measure Warm Reload
  console.log('\n----------------------------------------------------------------');
  console.log('Measuring Warm Start (Cache & Service Worker active)...');
  console.log('----------------------------------------------------------------\n');

  const warmBrowser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const warmPage = await warmBrowser.newPage();
  await warmPage.setViewport({ width: 1280, height: 900 });

  // Prime cache
  await warmPage.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 120000 });
  const primeIframe = await warmPage.waitForSelector('iframe', { timeout: 30000 });
  const primeFrame = await primeIframe.contentFrame();
  await primeFrame.waitForSelector('#sample_ds', { timeout: 120000 });

  // Now record reload
  const warmReqs = [];
  warmPage.on('requestfinished', req => {
    const resp = req.response();
    warmReqs.push({
      url: req.url(),
      fromCache: resp ? resp.fromCache() : false,
      fromSW: resp ? resp.fromServiceWorker() : false,
      size: resp ? parseInt(resp.headers()['content-length'] || '0', 10) : 0
    });
  });

  const warmT0 = Date.now();
  await warmPage.reload({ waitUntil: 'load', timeout: 120000 });
  const warmIframeElem = await warmPage.waitForSelector('iframe', { timeout: 30000 });
  const warmFrame = await warmIframeElem.contentFrame();
  await warmFrame.waitForSelector('#sample_ds', { timeout: 120000 });
  const ttiWarm = Date.now() - warmT0;
  console.log(`Warm Start TTI: ${(ttiWarm / 1000).toFixed(3)}s`);

  await warmBrowser.close();

  // Aggregate and categorize
  const reqList = Array.from(requests.values());
  const categories = {
    '1. WebR Core (R.wasm, R.js, libRlapack, library.data.gz)': reqList.filter(r =>
      r.url.includes('webr') && (
        r.url.includes('R.wasm') || r.url.includes('R.js') || r.url.includes('libR') ||
        r.url.includes('library.data.gz') || r.url.includes('webr-worker.js') || r.url.includes('library.js.metadata')
      )
    ),
    '2. R Packages (.tgz files)': reqList.filter(r => r.url.includes('/packages/') && r.url.endsWith('.tgz')),
    '3. WebR VFS System Data (translations, etc.)': reqList.filter(r => r.url.includes('/vfs/') || (r.url.includes('/packages/') && !r.url.endsWith('.tgz'))),
    '4. App Source & Bundled Assets (app.json)': reqList.filter(r => r.url.endsWith('app.json')),
    '5. Shinylive Framework JS/CSS': reqList.filter(r => r.url.includes('/shinylive/') && !r.url.includes('/webr/')),
    '6. Shiny Virtual Server & UI Widgets (iframe internal)': reqList.filter(r => r.url.includes('/app_') || r.url.includes('shinylive-inject-socket')),
    '7. HTML / Root Assets (index.html, favicon.ico)': reqList.filter(r => !r.url.includes('/shinylive/') && !r.url.includes('/app_') && !r.url.endsWith('app.json'))
  };

  // Compute breakdown stats
  let totalBytes = 0;
  const categoryStats = [];
  for (const [name, items] of Object.entries(categories)) {
    const bytes = items.reduce((sum, i) => sum + (i.size || 0), 0);
    const count = items.length;
    totalBytes += bytes;
    categoryStats.push({ name, count, bytes, mb: (bytes / (1024 * 1024)).toFixed(2), kb: (bytes / 1024).toFixed(0) });
  }

  // Top individual files
  const topFiles = [...reqList].sort((a, b) => (b.size || 0) - (a.size || 0)).slice(0, 15);

  const report = {
    summary: {
      coldStartTimeMs: ttiCold,
      coldStartTimeSec: +(ttiCold / 1000).toFixed(2),
      warmStartTimeMs: ttiWarm,
      warmStartTimeSec: +(ttiWarm / 1000).toFixed(2),
      totalTransferredMB: +(totalBytes / (1024 * 1024)).toFixed(2),
      totalTransferredBytes: totalBytes,
      totalRequests: reqList.length
    },
    categoryStats,
    topFiles: topFiles.map(f => ({
      url: f.url.replace('http://localhost:8100/', ''),
      sizeBytes: f.size,
      sizeKB: +(f.size / 1024).toFixed(1),
      sizeMB: +(f.size / (1024 * 1024)).toFixed(2),
      durationMs: f.durationMs
    })),
    timeline,
    outerPerf,
    iframePerf
  };

  fs.writeFileSync('test_browser/detailed_profiling_report.json', JSON.stringify(report, null, 2));

  console.log('\n================================================================');
  console.log('                     FINAL MEASUREMENT REPORT                   ');
  console.log('================================================================');
  console.log(`Cold Start TTI: ${report.summary.coldStartTimeSec} s`);
  console.log(`Warm Start TTI: ${report.summary.warmStartTimeSec} s`);
  console.log(`Total Download Size (Cold): ${report.summary.totalTransferredMB} MB across ${report.summary.totalRequests} requests`);
  console.log('\n--- Category Breakdown ---');
  categoryStats.forEach(c => {
    console.log(`${c.name.padEnd(55, ' ')} : ${c.mb.padStart(6, ' ')} MB (${c.count} files)`);
  });

  console.log('\n--- Top 10 Largest Files ---');
  report.topFiles.slice(0, 10).forEach((f, i) => {
    console.log(`${(i + 1).toString().padStart(2, ' ')}. ${f.sizeMB.toFixed(2).padStart(6, ' ')} MB | ${f.url} (${f.durationMs}ms)`);
  });
}

runDetailedProfile().catch(err => {
  console.error('Profiling error:', err);
  process.exit(1);
});
