// Browser edge-case check: uploaded CSV with a constant column (k) and a perfectly collinear column (ab = a + b).
// The app must stay connected, show actionable messages, and Optimize must not crash.
// Usage: node test_browser/test_optimize_edge_data.js <csv path> [variant]
//   variant "bad"   : y ~ a + b + k + ab + e   (constant + exact linear dependency in the model)
//   variant "clean" : y ~ a + b + e            (constant/collinear columns present in the data but unused)
const puppeteer = require('puppeteer-core');
const path = require('path');

const CSV = process.argv[2];
const VARIANT = process.argv[3] || 'bad';
const SHOT_DIR = process.env.SHOT_DIR || path.join(__dirname, 'shots');
const sleep = ms => new Promise(r => setTimeout(r, ms));
let failures = 0;
const ok = (c, m) => { console.log((c ? 'PASS' : 'FAIL') + ' - ' + m); if (!c) failures++; };

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true, args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  const pageErrors = [];
  page.on('pageerror', e => { pageErrors.push(e.message); console.error('PAGE ERROR:', e.message); });
  await page.goto('http://127.0.0.1:8100', { waitUntil: 'networkidle2', timeout: 60000 });
  const fileInput = await page.waitForSelector('input[type="file"]', { timeout: 30000 });
  await fileInput.uploadFile(path.resolve(CSV));
  await page.waitForFunction(() => document.querySelector('.modal-dialog') === null, { timeout: 30000 }).catch(() => {});
  await page.waitForSelector('#datatable table', { timeout: 20000 });
  await sleep(1500);
  await page.evaluate(() => { for (const t of document.querySelectorAll('a[data-toggle="tab"]')) if (t.innerText.trim() === 'Model') { t.click(); break; } });
  await page.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await sleep(1500);

  const wanted = VARIANT === 'bad' ? ['a', 'b', 'k', 'ab', 'e'] : ['a', 'b', 'e'];
  const colsNow = await page.evaluate(() => {
    const c = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets?.getInstance(c)?.hot; if (!hot && window.jQuery) hot = window.jQuery(c).data('hot');
    return hot.getSettings().colHeaders;
  });
  // The app leaves constant columns (zero variance) out of the structural matrix by itself.
  const preds = wanted.filter(v => colsNow.includes(v));
  ok(!wanted.includes('k') || !colsNow.includes('k'), 'constant column k is not offered in the structural model');
  const hdrs = await page.evaluate((preds) => {
    const c = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets?.getInstance(c)?.hot; if (!hot && window.jQuery) hot = window.jQuery(c).data('hot');
    const data = hot.getData(), headers = hot.getSettings().colHeaders;
    const row = data.findIndex(r => r[0] === 'y');
    hot.setDataAtCell(preds.map(v => [row, headers.indexOf(v), true]), 'edit');
    return headers;
  }, preds);
  console.log('Matrix columns:', hdrs.join(','));
  await page.waitForFunction((p) => (document.getElementById('lavaan_model')?.innerText || '').includes('y ~ ' + p.join(' + ')), { timeout: 20000 }, preds);
  await page.evaluate(() => document.getElementById('run_model').click());
  await sleep(6000);

  const alive = () => page.evaluate(() => !!(window.Shiny && Shiny.shinyapp && Shiny.shinyapp.isConnected()) && !document.getElementById('shiny-disconnected-overlay'));
  const state = await page.evaluate(() => ({
    notes: Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.innerText.trim()),
    alerts: Array.from(document.querySelectorAll('.alert, .shiny-output-error')).map(n => n.innerText.trim()).filter(Boolean),
    page: document.body.innerText,
    optimizeVisible: (() => { const b = document.getElementById('prune_model_btn'); return !!b && getComputedStyle(b).display !== 'none'; })()
  }));
  console.log('After Run:', JSON.stringify({ ...state, page: undefined }).slice(0, 900));
  ok(await alive(), 'app still connected after Run');
  await page.screenshot({ path: path.join(SHOT_DIR, `edge_${VARIANT}_run.png`) });

  if (VARIANT === 'bad') {
    ok(state.alerts.concat(state.notes, [state.page]).some(t => /linearly dependent|constant|zero variance|identified|Remove/i.test(t)),
       'an actionable message about the constant / collinear variables is shown');
  }
  if (state.optimizeVisible) {
    await page.evaluate(() => document.getElementById('prune_model_btn').click());
    await sleep(3000);
    const modal = await page.evaluate(() => document.querySelector('.modal-body')?.innerText.slice(0, 400) || '');
    console.log('Optimize modal:', modal.replace(/\s+/g, ' '));
    ok(await alive(), 'app still connected after opening Optimize');
    if (await page.$('#run_prune_explore')) {
      await page.evaluate(() => document.getElementById('run_prune_explore').click());
      await sleep(15000);
      ok(await alive(), 'app still connected after running Optimize');
      const res = await page.evaluate(() => ({
        rows: Array.from(document.querySelectorAll('#prune_candidates_table tbody tr')).slice(0, 4).map(tr => tr.innerText.replace(/\s+/g, ' ')),
        alerts: Array.from(document.querySelectorAll('.modal-body .alert')).map(n => n.innerText.trim())
      }));
      console.log('Optimize result:', JSON.stringify(res).slice(0, 900));
      await page.screenshot({ path: path.join(SHOT_DIR, `edge_${VARIANT}_optimize.png`) });
    }
  } else {
    console.log('Optimize button not available for this state (expected for an invalid baseline).');
  }
  ok(pageErrors.length === 0, 'no JavaScript page errors (' + pageErrors.length + ')');
  await browser.close();
  console.log(failures ? `FAILED: ${failures}` : 'ALL PASSED');
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error('SCRIPT ERROR:', e); process.exit(2); });
