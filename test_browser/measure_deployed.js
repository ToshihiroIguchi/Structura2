// Measures Structura2 startup milestones (cold starts in fresh browser profiles + one warm reload).
//
// Usage: node test_browser/measure_deployed.js [baseUrl] [coldRuns]
//   baseUrl   default http://localhost:8100 (e.g. https://toshihiroiguchi.github.io/Structura2/)
//   coldRuns  default 3
//
// Milestones come from host/splash.js (window.__structuraMilestones, ms since page start). Finer
// stages reported by app.R through the `structura-progress` BroadcastChannel (e.g. lavaan_loaded,
// which arrives after `ready`) are collected as well.
//
// NOTE: wall-clock numbers depend heavily on machine load (background jobs make WebAssembly
// compilation, e.g. dyn.load of shared libraries, several times slower). Compare builds by
// alternating runs in one session, and read the deltas between stages rather than absolute times.
const puppeteer = require('puppeteer-core');

const BASE_URL = process.argv[2] || 'http://localhost:8100';
const COLD_RUNS = parseInt(process.argv[3] || '3', 10);
const EDGE = 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe';
const STAGES = ['page', 'worker', 'runtime', 'app_start', 'libs_attached', 'ui_built',
                'session_start', 'ready', 'lavaan_loaded'];

async function measureOnce(browser, { cacheEnabled }) {
  const page = await browser.newPage();
  await page.setCacheEnabled(cacheEnabled);
  await page.evaluateOnNewDocument(() => {
    window.__stages = {};
    const t0 = Date.now();
    const channel = new BroadcastChannel('structura-progress');
    channel.onmessage = (e) => {
      if (e.data && e.data.stage && !(e.data.stage in window.__stages)) {
        window.__stages[e.data.stage] = Date.now() - t0;
      }
    };
  });
  await page.goto(BASE_URL, { waitUntil: 'load', timeout: 180000 });
  await page.waitForFunction(
    () => window.__structuraMilestones && window.__structuraMilestones.ready !== undefined,
    { timeout: 600000, polling: 250 });
  // lavaan is attached right after the Load Data dialog is shown
  await page.waitForFunction(() => window.__stages.lavaan_loaded !== undefined,
                             { timeout: 120000, polling: 250 }).catch(() => {});
  const result = await page.evaluate(() =>
    Object.assign({}, window.__stages, window.__structuraMilestones));
  await page.close();
  return result;
}

function printRun(label, m) {
  const cells = STAGES.map((s) => `${s}=${m[s] !== undefined ? (m[s] / 1000).toFixed(1) : '-'}`);
  console.log(`${label.padEnd(8)} ${cells.join('  ')}`);
}

(async () => {
  console.log(`Target: ${BASE_URL}  (cold runs: ${COLD_RUNS})`);
  const cold = [];
  for (let i = 0; i < COLD_RUNS; i++) {
    // A fresh browser per cold run = empty HTTP cache, no service worker
    const browser = await puppeteer.launch({ executablePath: EDGE, headless: true, args: ['--no-sandbox'] });
    const m = await measureOnce(browser, { cacheEnabled: false });
    printRun(`cold #${i + 1}`, m);
    cold.push(m);
    await browser.close();
  }

  // Warm: same profile, second visit (HTTP cache + service worker)
  const browser = await puppeteer.launch({ executablePath: EDGE, headless: true, args: ['--no-sandbox'] });
  await measureOnce(browser, { cacheEnabled: true });
  const warm = await measureOnce(browser, { cacheEnabled: true });
  printRun('warm', warm);
  await browser.close();

  const median = (f) => {
    const a = cold.map(f).filter((v) => v !== undefined).sort((x, y) => x - y);
    return a.length ? a[Math.floor(a.length / 2)] : undefined;
  };
  console.log('\nCold-start median deltas (seconds):');
  const span = (from, to) => median((m) => (m[to] !== undefined && m[from] !== undefined) ? m[to] - m[from] : undefined);
  for (const [from, to] of [['page', 'runtime'], ['runtime', 'app_start'], ['app_start', 'libs_attached'],
                            ['libs_attached', 'ui_built'], ['ui_built', 'ready'], ['ready', 'lavaan_loaded']]) {
    const v = span(from, to);
    console.log(`  ${from} -> ${to}`.padEnd(32) + (v !== undefined ? (v / 1000).toFixed(1) : '-'));
  }
})().catch((err) => { console.error('measure_deployed failed:', err); process.exit(1); });
