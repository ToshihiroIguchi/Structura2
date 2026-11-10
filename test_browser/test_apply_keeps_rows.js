// Browser check: with listwise deletion, applying an optimization candidate that removes a predictor keeps the
// rows used while optimizing, and the default optimization criterion is BIC.
// Usage (server on port 8100): node test_browser/test_apply_keeps_rows.js <csv with x1,x2,x3,y> <rows_all> <rows_without_x3>
const puppeteer = require('puppeteer-core');
const path = require('path');
const CSV = process.argv[2], N_ALL = +process.argv[3], N_FREE = +process.argv[4];
const SHOT_DIR = process.env.SHOT_DIR || path.join(__dirname, 'shots');
require('fs').mkdirSync(SHOT_DIR, { recursive: true });
const sleep = ms => new Promise(r => setTimeout(r, ms));
let failures = 0;
const ok = (c, m) => { console.log((c ? 'PASS' : 'FAIL') + ' - ' + m); if (!c) failures++; };

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:/Program Files (x86)/Microsoft/Edge/Application/msedge.exe',
    headless: true, args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  const errs = [];
  page.on('pageerror', e => { errs.push(e.message); console.error('PAGE ERROR:', e.message); });
  await page.goto('http://127.0.0.1:8100', { waitUntil: 'networkidle2', timeout: 60000 });
  const fi = await page.waitForSelector('input[type="file"]', { timeout: 30000 });
  await fi.uploadFile(path.resolve(CSV));
  await page.waitForSelector('#datatable table', { timeout: 20000 });
  await sleep(1500);
  console.log('missing_method =', await page.evaluate(() => document.getElementById('missing_method')?.value));
  await page.evaluate(() => { for (const t of document.querySelectorAll('a[data-toggle="tab"]')) if (t.innerText.trim() === 'Model') { t.click(); break; } });
  await page.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await sleep(1500);
  await page.evaluate(() => {
    const c = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets?.getInstance(c)?.hot; if (!hot && window.jQuery) hot = window.jQuery(c).data('hot');
    const data = hot.getData(), h = hot.getSettings().colHeaders, row = data.findIndex(r => r[0] === 'y');
    hot.setDataAtCell(['x1', 'x2', 'x3'].map(v => [row, h.indexOf(v), true]), 'edit');
  });
  await page.waitForFunction(() => (document.getElementById('lavaan_model')?.innerText || '').includes('y ~ x1 + x2 + x3'), { timeout: 20000 });
  const nobs = () => page.evaluate(() => {
    const m = document.body.innerText.match(/\b(?:nobs|N|Observations|Number of observations)\b[^\d]{0,20}(\d+)/i);
    return m ? +m[1] : null;
  });
  await page.evaluate(() => document.getElementById('run_model').click());
  await page.waitForFunction(() => { const b = document.getElementById('prune_model_btn'); return b && getComputedStyle(b).display !== 'none'; }, { timeout: 30000 });
  await sleep(2000);
  console.log('N after baseline run:', await nobs());

  await page.evaluate(() => document.getElementById('prune_model_btn').click());
  await page.waitForSelector('#prune_criterion', { timeout: 15000 });
  const crit = await page.evaluate(() => { const s = document.getElementById('prune_criterion'); return s ? (s.selectize ? s.selectize.getValue() : s.value) : null; });
  ok(crit === 'BIC', 'default optimization criterion is BIC (got ' + crit + ')');
  await page.evaluate(() => {
    if (window.Shiny) Shiny.setInputValue('prune_strategy', 'exhaustive');
    const sel = document.getElementById('prune_strategy'); if (sel && sel.selectize) sel.selectize.setValue('exhaustive');
  });
  await sleep(800);
  await page.evaluate(() => document.getElementById('run_prune_explore').click());
  await page.waitForSelector('#prune_candidates_table tbody tr', { timeout: 120000 });
  await sleep(1500);
  const top = await page.evaluate(() => Array.from(document.querySelector('#prune_candidates_table tbody tr').querySelectorAll('td')).map(t => t.innerText.trim()).join(' | '));
  console.log('Rank 1:', top);
  ok(!/x3/.test((top.match(/\|[^|]*$/) || [''])[0]) || true, 'rank 1 read');

  await page.evaluate(() => document.getElementById('apply_pruned_model').click());
  await sleep(8000);
  const after = await page.evaluate(() => ({
    syntax: document.getElementById('lavaan_model').innerText,
    notes: Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.innerText.trim()),
    fitText: document.body.innerText
  }));
  await page.screenshot({ path: path.join(SHOT_DIR, 'apply_keeps_rows.png') });
  console.log('Syntax:', after.syntax.replace(/\n/g, ' ; '));
  console.log('Notes:', JSON.stringify(after.notes));
  const n2 = await nobs();
  console.log('N after apply:', n2);
  ok(!/\bx3\b/.test(after.syntax), 'x3 removed from the applied model');
  ok(after.notes.some(n => n.includes('Rows kept at ' + N_ALL)), 'notification says rows kept at ' + N_ALL);
  ok(n2 === N_ALL, `applied model is estimated on ${N_ALL} rows (not ${N_FREE}); got ${n2}`);
  ok(errs.length === 0, 'no JS page errors');
  await browser.close();
  console.log(failures ? 'FAILED: ' + failures : 'ALL PASSED');
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error('SCRIPT ERROR:', e); process.exit(2); });
