// Browser check: Auto-Optimize may strip an exogenous predictor of every path (scored with fixed-zero
// paths) and applying such a candidate removes the predictor from the model.
//
// Usage (server must run on port 8100, from the project root or test_browser/):
//   node test_browser/test_optimize_exclude_var.js [strategy] [scenario]
//   strategy: exhaustive | stepwise | sa | adaptive   (default exhaustive)
//   scenario: A (x6 ~ x1+..+x5, default) | D (x1 protected via Variable Isolation Prevention) | CANCEL
// SHINYLIVE=1 targets the exported Shinylive site (iframe) instead of runApp.
const puppeteer = require('puppeteer-core');
const { execFileSync } = require('child_process');
const path = require('path');
const fs = require('fs');

const STRATEGY = process.argv[2] || 'exhaustive';
const SCENARIO = (process.argv[3] || 'A').toUpperCase();
const SHINYLIVE = !!process.env.SHINYLIVE;
const URL = SHINYLIVE ? 'http://localhost:8100' : 'http://127.0.0.1:8100';
const TMO = SHINYLIVE ? 120000 : 20000;
const SHOT_DIR = process.env.SHOT_DIR || path.join(__dirname, 'shots');
fs.mkdirSync(SHOT_DIR, { recursive: true });
const RSCRIPT = 'C:\\Program Files\\R\\R-4.6.0\\bin\\Rscript.exe';

let failures = 0;
const ok = (cond, msg) => { console.log((cond ? 'PASS' : 'FAIL') + ' - ' + msg); if (!cond) failures++; };
const sleep = ms => new Promise(r => setTimeout(r, ms));

// Reference: best non-empty subset of x1..x5 for x6 by plain regression AIC, relative to the full model.
function expectedBest(protect) {
  const code = `
    d <- lavaan::HolzingerSwineford1939; v <- paste0('x', 1:5)
    full <- AIC(lm(x6 ~ ., d[, c(v, 'x6')]))
    best <- Inf; bs <- NULL
    for (k in 1:31) { s <- v[bitwAnd(k, 2^(0:4)) > 0]
      ${protect ? "if (!('x1' %in% s)) next" : ''}
      a <- AIC(lm(reformulate(s, 'x6'), d)) - full
      if (a < best) { best <- a; bs <- s } }
    cat(sprintf('%.2f|%s', best, paste(bs, collapse=',')))`;
  const tmp = path.join(require('os').tmpdir(), 'structura2_expected_best.R');
  fs.writeFileSync(tmp, code);
  const out = execFileSync(RSCRIPT, [tmp]).toString().trim().split('|');
  return { delta: parseFloat(out[0]), subset: out[1].split(',') };
}

(async () => {
  console.log(`Strategy=${STRATEGY} Scenario=${SCENARIO} ${SHINYLIVE ? '(Shinylive)' : '(runApp)'}`);
  const exp = expectedBest(SCENARIO === 'D');
  console.log('Reference best subset (lm):', exp.subset.join(' + '), 'delta AIC =', exp.delta.toFixed(2));

  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true, args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 1000 });
  const pageErrors = [];
  page.on('pageerror', err => { pageErrors.push(err.message); console.error('PAGE ERROR:', err.message); });

  await page.goto(URL, { waitUntil: 'networkidle2', timeout: 60000 });
  let ui = page;
  if (SHINYLIVE) {
    let fr = null;
    for (let i = 0; i < 90 && !fr; i++) { fr = page.frames().find(f => f !== page.mainFrame()); if (!fr) await sleep(1000); }
    if (!fr) { console.error('No Shinylive iframe'); process.exit(1); }
    ui = fr;
  }

  await ui.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: TMO });
  await ui.evaluate(() => document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]').click());
  await ui.waitForSelector('#datatable', { timeout: TMO });
  await ui.evaluate(() => {
    for (const t of document.querySelectorAll('a[data-toggle="tab"]')) if (t.innerText.trim() === 'Model') { t.click(); break; }
  });
  await ui.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: TMO });
  await sleep(1000);

  // x6 ~ x1 + x2 + x3 + x4 + x5
  await ui.evaluate(() => {
    const c = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(c)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(c).data('hot');
    const data = hot.getData(), headers = hot.getSettings().colHeaders;
    const row = data.findIndex(r => r[0] === 'x6');
    const edits = [];
    for (const v of ['x1', 'x2', 'x3', 'x4', 'x5']) edits.push([row, headers.indexOf(v), true]);
    hot.setDataAtCell(edits, 'edit');
  });
  await ui.waitForFunction(() => (document.getElementById('lavaan_model')?.innerText || '').includes('x6 ~ x1 + x2 + x3 + x4 + x5'), { timeout: 20000 });
  await ui.evaluate(() => document.getElementById('run_model').click());
  await ui.waitForFunction(() => {
    const b = document.getElementById('prune_model_btn');
    return b && getComputedStyle(b).display !== 'none';
  }, { timeout: TMO });
  await sleep(1500);
  const baseAic = await ui.evaluate(() => {
    const rows = Array.from(document.querySelectorAll('table tr')).filter(tr => tr.innerText.trim().startsWith('AIC'));
    return rows.length ? rows[0].innerText.replace(/\s+/g, ' ') : '';
  });
  console.log('Baseline AIC row:', baseAic);
  const syntaxBefore = await ui.evaluate(() => document.getElementById('lavaan_model').innerText);

  // Open Auto-Optimize, choose strategy, optionally protect x1
  await ui.evaluate(() => document.getElementById('prune_model_btn').click());
  await ui.waitForSelector('#prune_lock_table', { timeout: 15000 });
  await ui.evaluate((s) => {
    if (window.Shiny) window.Shiny.setInputValue('prune_strategy', s);
    const sel = document.getElementById('prune_strategy');
    if (sel && sel.selectize) sel.selectize.setValue(s);
    else if (sel) { sel.value = s; sel.dispatchEvent(new Event('change', { bubbles: true })); }
  }, STRATEGY);
  if (SCENARIO === 'D') {
    await ui.evaluate(() => {
      const cb = document.querySelector('input[name="prune_retain_preds"][value="x1"]');
      if (cb && !cb.checked) cb.click();
    });
  }
  await sleep(800);
  await ui.evaluate(() => document.getElementById('run_prune_explore').click());
  await ui.waitForSelector('#prune_candidates_table tbody tr', { timeout: SHINYLIVE ? 600000 : 240000 });
  await sleep(1500);
  await page.screenshot({ path: path.join(SHOT_DIR, `result_${SCENARIO}_${STRATEGY}.png`) });

  const res = await ui.evaluate(() => ({
    headers: Array.from(document.querySelectorAll('#prune_candidates_table thead th')).map(th => th.innerText.trim()),
    rows: Array.from(document.querySelectorAll('#prune_candidates_table tbody tr')).map(tr =>
      Array.from(tr.querySelectorAll('td')).map(td => td.innerText.trim())),
    modalText: document.querySelector('.modal-body')?.innerText || ''
  }));
  res.rows.slice(0, 8).forEach(r => console.log('ROW:', r.join(' | ')));
  const col = n => res.headers.indexOf(n);
  const top = res.rows[0];
  const statuses = res.rows.map(r => r[col('Status')]);
  ok(!statuses.some(s => s.includes('[Variable Dropped]')), 'no candidate is labelled [Variable Dropped]');
  ok(top[col('Status')].includes('[Optimal]'), 'rank 1 is [Optimal]: ' + top[col('Status')]);
  ok(top[col('ΔAIC')] !== '—', 'rank 1 has a numeric ΔAIC: ' + top[col('ΔAIC')]);

  if (STRATEGY === 'exhaustive' || STRATEGY === 'adaptive') {
    const got = parseFloat(top[col('ΔAIC')]);
    ok(Math.abs(got - exp.delta) < 0.011, `rank 1 ΔAIC ${got} equals lm best-subset ΔAIC ${exp.delta}`);
    const retained = (top[col('Retained Paths')].match(/x[1-5]/g) || []).sort().join(',');
    ok(retained === exp.subset.slice().sort().join(','), `rank 1 retained predictors ${retained} = lm best subset ${exp.subset.join(',')}`);
  }
  const hasExcl = !exp.subset.includes('x1') || exp.subset.length < 5;
  if (SCENARIO === 'D') {
    ok(!res.rows.some(r => !/x1/.test(r[col('Retained Paths')])), 'protected x1 is never stripped of all paths');
  }
  // Other predictors (e.g. x2) can still lose every path in scenario D, so the notice is expected there too.
  ok(res.modalText.includes('scored with its removed paths fixed to zero') === hasExcl,
     'fixed-zero notice shown when a predictor can be removed');

  if (SCENARIO === 'CANCEL') {
    await ui.evaluate(() => { const b = Array.from(document.querySelectorAll('.modal-footer button, .modal-header button')).find(x => /Close/.test(x.innerText)); if (b) b.click(); });
    await sleep(1500);
    const after = await ui.evaluate(() => document.getElementById('lavaan_model').innerText);
    ok(after === syntaxBefore, 'closing the result modal leaves the model unchanged');
  } else {
    // Apply rank 1
    await ui.evaluate(() => document.getElementById('apply_pruned_model').click());
    await ui.waitForFunction(() => document.getElementById('lavaan_model').innerText.length > 0 &&
      !document.querySelector('#prune_candidates_table'), { timeout: 30000 });
    await sleep(6000);
    const after = await ui.evaluate(() => ({
      syntax: document.getElementById('lavaan_model').innerText,
      notes: Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.innerText.trim()),
      svgText: Array.from(document.querySelectorAll('svg text')).map(t => t.textContent.trim()).join(' '),
      matrix: (() => { const c = document.getElementById('checkbox_matrix'); let hot = window.HTMLWidgets?.getInstance(c)?.hot; if (!hot && window.jQuery) hot = window.jQuery(c).data('hot'); return { data: hot.getData(), headers: hot.getSettings().colHeaders }; })()
    }));
    await page.screenshot({ path: path.join(SHOT_DIR, `applied_${SCENARIO}_${STRATEGY}.png`) });
    console.log('Syntax after apply:', after.syntax.replace(/\n/g, ' ; '));
    console.log('Notifications:', JSON.stringify(after.notes));
    const removed = ['x1', 'x2', 'x3', 'x4', 'x5'].filter(v => !exp.subset.includes(v));
    if (removed.length && (STRATEGY === 'exhaustive' || STRATEGY === 'adaptive')) {
      ok(after.syntax.trim() === 'x6 ~ ' + exp.subset.join(' + '), 'applied syntax = ' + 'x6 ~ ' + exp.subset.join(' + '));
      ok(after.notes.some(n => n.includes('Removed from the model') && removed.every(v => n.includes(v))), 'warning notification names the removed predictors');
      removed.forEach(v => ok(!new RegExp('\\b' + v + '\\b').test(after.syntax), `${v} is gone from the applied syntax`));
      const r6 = after.matrix.data.find(r => r[0] === 'x6');
      removed.forEach(v => ok(r6[after.matrix.headers.indexOf(v)] === false, `structural table: x6 ~ ${v} unchecked`));
    }
    ok(!after.notes.some(n => n.includes('differs from the candidate table')), 'no false AIC-mismatch warning after applying');

    // PDF report: the optimization history must mention the removed predictors (window.print is stubbed)
    await ui.evaluate(() => { window.print = () => { window.__printed = true; }; });
    await ui.evaluate(() => document.getElementById('export_pdf_btn').click());
    await ui.waitForFunction(() => window.__printed === true, { timeout: 30000 }).catch(() => {});
    const report = await ui.evaluate(() => document.getElementById('structura-print-report')?.innerText || '');
    ok(report.includes('Model Optimization History'), 'PDF report contains the optimization history section');
    if (removed.length) {
      ok(report.includes('Predictors removed from the model') && removed.every(v => report.includes(v)),
         'PDF optimization history lists the removed predictors: ' + (report.match(/Predictors removed from the model.*/) || [''])[0].slice(0, 120));
    }
  }
  ok(pageErrors.length === 0, 'no JavaScript page errors (' + pageErrors.length + ')');
  await browser.close();
  console.log(failures ? `FAILED: ${failures}` : 'ALL PASSED');
  process.exit(failures ? 1 : 0);
})().catch(e => { console.error('SCRIPT ERROR:', e); process.exit(2); });
