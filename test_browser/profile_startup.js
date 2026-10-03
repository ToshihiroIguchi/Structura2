const puppeteer = require('puppeteer-core');
const fs = require('fs');
const path = require('path');

async function measureRun(runName, options = { clearCache: true }) {
  console.log(`\n========================================`);
  console.log(`Starting Run: ${runName}`);
  console.log(`Options: Cache ${options.clearCache ? 'DISABLED (Cold Start)' : 'ENABLED (Warm Start)'}`);
  console.log(`========================================\n`);

  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  if (options.clearCache) {
    await page.setCacheEnabled(false);
  } else {
    await page.setCacheEnabled(true);
  }

  const startTime = Date.now();
  const events = [];
  const requests = new Map();

  const recordEvent = (category, name, details = {}) => {
    const elapsed = Date.now() - startTime;
    events.push({ timeMs: elapsed, category, name, details });
    console.log(`[${(elapsed / 1000).toFixed(3)}s] [${category}] ${name}`);
  };

  recordEvent('PHASE', 'Browser and Page initialized');

  // Track console logs
  page.on('console', msg => {
    const text = msg.text();
    recordEvent('CONSOLE', text.substring(0, 120));
  });

  page.on('pageerror', err => {
    recordEvent('PAGE_ERROR', err.message);
  });

  // Track network requests
  page.on('request', req => {
    const url = req.url();
    requests.set(url, {
      url,
      method: req.method(),
      resourceType: req.resourceType(),
      startTime: Date.now(),
      endTime: null,
      durationMs: null,
      size: 0,
      status: null
    });
  });

  page.on('requestfinished', req => {
    const url = req.url();
    const entry = requests.get(url);
    if (entry) {
      entry.endTime = Date.now();
      entry.durationMs = entry.endTime - entry.startTime;
      const resp = req.response();
      if (resp) {
        entry.status = resp.status();
        const headers = resp.headers();
        entry.size = parseInt(headers['content-length'] || '0', 10);
      }
      const shortUrl = url.replace('http://localhost:8100', '');
      if (entry.durationMs > 100 || entry.size > 100000 || url.includes('.wasm') || url.includes('.tgz') || url.includes('.data.gz') || url.includes('app.json')) {
        recordEvent('NETWORK', `Finished: ${shortUrl.substring(0, 70)} (${entry.durationMs}ms, ${(entry.size / 1024).toFixed(1)} KB)`);
      }
    }
  });

  page.on('requestfailed', req => {
    const url = req.url();
    const entry = requests.get(url);
    if (entry) {
      entry.endTime = Date.now();
      entry.durationMs = entry.endTime - entry.startTime;
      entry.failed = true;
    }
    recordEvent('NETWORK_FAILED', url);
  });

  recordEvent('PHASE', 'Navigating to http://localhost:8100');
  await page.goto('http://localhost:8100', { waitUntil: 'load', timeout: 120000 });
  recordEvent('PHASE', 'Main HTML page load event fired');

  recordEvent('PHASE', 'Waiting for Shinylive iframe');
  let frame = null;
  try {
    await page.waitForSelector('iframe', { timeout: 30000 });
    const iframeElem = await page.$('iframe');
    frame = await iframeElem.contentFrame();
    recordEvent('PHASE', 'Shinylive iframe attached');
  } catch (e) {
    recordEvent('ERROR', `Iframe wait failed: ${e.message}`);
  }

  if (frame) {
    recordEvent('PHASE', 'Waiting for Shiny UI ready (#sample_ds modal)');
    try {
      await frame.waitForSelector('#sample_ds', { timeout: 120000 });
      recordEvent('PHASE', 'App Ready: #sample_ds modal displayed');
    } catch (e) {
      recordEvent('ERROR', `Modal wait failed: ${e.message}`);
    }
  }

  const totalTime = Date.now() - startTime;
  recordEvent('PHASE', `Run complete (Total: ${(totalTime / 1000).toFixed(3)}s)`);

  // Aggregate network statistics
  const reqList = Array.from(requests.values());
  const categories = {
    'HTML / Shinylive Framework JS/CSS': reqList.filter(r => r.url.includes('/shinylive/') && !r.url.includes('/webr/')),
    'App Source & Assets (app.json)': reqList.filter(r => r.url.endsWith('app.json')),
    'WebR Core (R.wasm, R.js, libRlapack, library.data.gz)': reqList.filter(r =>
      r.url.includes('webr') && (r.url.includes('R.wasm') || r.url.includes('R.js') || r.url.includes('libR') || r.url.includes('library.data.gz') || r.url.includes('webr-worker.js') || r.url.includes('library.js.metadata'))
    ),
    'R Packages (.tgz files)': reqList.filter(r => r.url.includes('/packages/') && r.url.endsWith('.tgz')),
    'WebR VFS Library Data (translations, etc.)': reqList.filter(r => r.url.includes('/vfs/') || (r.url.includes('/packages/') && !r.url.endsWith('.tgz'))),
    'Shiny App UI & Widgets (iframe resources)': reqList.filter(r => r.url.includes('/app_') || r.url.includes('shinylive-inject-socket')),
    'Other / Uncategorized': []
  };

  const categorizedUrls = new Set(Object.values(categories).flat().map(r => r.url));
  categories['Other / Uncategorized'] = reqList.filter(r => !categorizedUrls.has(r.url));

  await browser.close();

  return {
    runName,
    options,
    totalTimeMs: totalTime,
    events,
    reqList,
    categories
  };
}

(async () => {
  try {
    // 1. Cold Start Run
    const coldResults = await measureRun('Run 1: Cold Start (No Cache)', { clearCache: true });

    // 2. Warm Start Run (Immediately after, same browser profile if possible or standard navigation)
    // To measure warm start properly, we open browser, load once to populate cache/sw, then measure reload.
    console.log(`\n========================================`);
    console.log(`Setting up Warm Start test environment...`);
    console.log(`========================================\n`);
    
    const warmBrowser = await puppeteer.launch({
      executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
      headless: true,
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
    const prepPage = await warmBrowser.newPage();
    await prepPage.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 120000 });
    const prepIframeElem = await prepPage.waitForSelector('iframe', { timeout: 30000 });
    const prepFrame = await prepIframeElem.contentFrame();
    await prepFrame.waitForSelector('#sample_ds', { timeout: 120000 });
    console.log('Warm cache primed. Running Warm Reload measurement...');

    const warmStartTime = Date.now();
    const warmRequests = [];
    const warmEvents = [];
    prepPage.on('requestfinished', req => {
      const resp = req.response();
      const fromCache = resp ? resp.fromCache() : false;
      const fromSW = resp ? resp.fromServiceWorker() : false;
      warmRequests.push({
        url: req.url(),
        fromCache,
        fromSW,
        size: resp ? parseInt(resp.headers()['content-length'] || '0', 10) : 0
      });
    });

    // Reload page to measure warm start
    await prepPage.reload({ waitUntil: 'load', timeout: 120000 });
    warmEvents.push({ timeMs: Date.now() - warmStartTime, msg: 'Page reloaded' });
    const warmIframeElem = await prepPage.waitForSelector('iframe', { timeout: 30000 });
    const warmFrame = await warmIframeElem.contentFrame();
    warmEvents.push({ timeMs: Date.now() - warmStartTime, msg: 'Iframe loaded' });
    await warmFrame.waitForSelector('#sample_ds', { timeout: 120000 });
    const warmTotalMs = Date.now() - warmStartTime;
    warmEvents.push({ timeMs: warmTotalMs, msg: 'App Ready (#sample_ds modal displayed)' });

    await warmBrowser.close();

    // Generate Analysis Report
    console.log('\n\n======================================================');
    console.log('             STARTUP PERFORMANCE SUMMARY              ');
    console.log('======================================================\n');
    console.log(`Cold Start Time to Interactive: ${(coldResults.totalTimeMs / 1000).toFixed(2)} s (${coldResults.totalTimeMs} ms)`);
    console.log(`Warm Start Time to Interactive: ${(warmTotalMs / 1000).toFixed(2)} s (${warmTotalMs} ms)`);

    console.log('\n--- NETWORK BREAKDOWN BY CATEGORY (Cold Start) ---');
    let grandTotalBytes = 0;
    for (const [catName, list] of Object.entries(coldResults.categories)) {
      const catBytes = list.reduce((sum, r) => sum + (r.size || 0), 0);
      grandTotalBytes += catBytes;
      const count = list.length;
      console.log(`\n• ${catName}:`);
      console.log(`  Requests: ${count} | Total Size: ${(catBytes / (1024 * 1024)).toFixed(2)} MB (${(catBytes / 1024).toFixed(0)} KB)`);
      // Show top 3 largest in this category
      const topItems = [...list].sort((a, b) => (b.size || 0) - (a.size || 0)).slice(0, 4);
      topItems.forEach(item => {
        const urlEnd = item.url.split('/').slice(-2).join('/');
        console.log(`    - ${urlEnd}: ${(item.size / 1024).toFixed(1)} KB (download: ${item.durationMs}ms)`);
      });
    }
    console.log(`\nGRAND TOTAL TRANSFERRED (Cold Start): ${(grandTotalBytes / (1024 * 1024)).toFixed(2)} MB`);

    console.log('\n--- TOP 10 LARGEST ASSETS (Cold Start) ---');
    const sortedBySize = [...coldResults.reqList].sort((a, b) => (b.size || 0) - (a.size || 0)).slice(0, 10);
    sortedBySize.forEach((r, idx) => {
      const urlEnd = r.url.replace('http://localhost:8100/', '');
      console.log(`${idx + 1}. ${(r.size / (1024 * 1024)).toFixed(2)} MB (${(r.size / 1024).toFixed(0)} KB) - ${urlEnd} [took ${r.durationMs}ms]`);
    });

    // Save full JSON report for detailed inspection
    const reportPath = path.join(__dirname, 'startup_profiling_result.json');
    fs.writeFileSync(reportPath, JSON.stringify({
      cold: {
        totalTimeMs: coldResults.totalTimeMs,
        events: coldResults.events,
        requests: coldResults.reqList,
        categories: coldResults.categories
      },
      warm: {
        totalTimeMs: warmTotalMs,
        events: warmEvents,
        requests: warmRequests
      }
    }, null, 2));

    console.log(`\nDetailed profiling data saved to: ${reportPath}`);
  } catch (err) {
    console.error('Fatal profiling error:', err);
    process.exit(1);
  }
})();
