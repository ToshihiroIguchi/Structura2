const puppeteer = require('puppeteer-core');
const fs = require('fs');

const STRATEGY = process.argv[2] || 'stepwise';
(async () => {
  console.log('Running Auto-Optimize to completion with strategy: ' + STRATEGY + '');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1300, height: 950 });

  page.on('console', msg => console.log('BROWSER:', msg.text()));
  page.on('pageerror', err => console.error('PAGE ERROR:', err.message));

  console.log('Navigating to ' + (process.env.SHINYLIVE ? 'http://localhost:8100' : 'http://127.0.0.1:8100') + ' ...');
  await page.goto('' + (process.env.SHINYLIVE ? 'http://localhost:8100' : 'http://127.0.0.1:8100') + '', { waitUntil: 'networkidle2', timeout: 30000 });

  let ui = page;
  if (process.env.SHINYLIVE) {
    let fr = null;
    for (let i = 0; i < 60 && !fr; i++) {
      fr = page.frames().find(f => f !== page.mainFrame());
      if (!fr) await new Promise(r => setTimeout(r, 1000));
    }
    if (!fr) { console.error('No Shinylive iframe'); process.exit(1); }
    ui = fr;
  }
  console.log('Selecting HolzingerSwineford1939 demo dataset...');
  await ui.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: process.env.SHINYLIVE ? 120000 : 15000 });
  await ui.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  console.log('Waiting for data table...');
  await ui.waitForSelector('#datatable', { timeout: process.env.SHINYLIVE ? 120000 : 15000 });
  console.log('Data loaded. Switching to Model tab...');

  await ui.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await ui.waitForSelector('#input_table td input[type="checkbox"]', { timeout: process.env.SHINYLIVE ? 120000 : 15000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Configuring measurement model for visual and textual...');
  await ui.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtCell([
        [0, 0, 'visual'],
        [0, 8, true],  // x1
        [0, 9, true],  // x2
        [0, 10, true], // x3
        [1, 0, 'textual'],
        [1, 11, true], // x4
        [1, 12, true], // x5
        [1, 13, true], // x6
        [2, 0, 'speed'],
        [2, 14, true], // x7
        [2, 15, true], // x8
        [2, 16, true]  // x9
      ], 'edit');
    }
  });

  await ui.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('visual =~') && text.includes('textual =~') && text.includes('speed =~');
  }, { timeout: 20000 });

  console.log('Configuring structural paths (textual ~ visual, speed ~ visual, speed ~ textual)...');
  await ui.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      const data = hot.getData();
      const headers = hot.getSettings().colHeaders;
      let textualIdx = -1, speedIdx = -1;
      let visualCol = -1, textualCol = -1;
      for (let r = 0; r < data.length; r++) {
        if (data[r][0] === 'textual') textualIdx = r;
        if (data[r][0] === 'speed') speedIdx = r;
      }
      for (let c = 0; c < headers.length; c++) {
        if (headers[c] === 'visual') visualCol = c;
        if (headers[c] === 'textual') textualCol = c;
      }
      const edits = [];
      if (textualIdx >= 0 && visualCol >= 0) edits.push([textualIdx, visualCol, true]);
      if (speedIdx >= 0 && visualCol >= 0) edits.push([speedIdx, visualCol, true]);
      if (speedIdx >= 0 && textualCol >= 0) edits.push([speedIdx, textualCol, true]);
      hot.setDataAtCell(edits, 'edit');
    }
  });

  await ui.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('textual ~ visual') && text.includes('speed ~ visual');
  }, { timeout: 20000 });

  console.log('Clicking Run / Update Model...');
  await ui.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for baseline model to fit and prune_model_btn to appear...');
  await ui.waitForFunction(() => {
    const btn = document.getElementById('prune_model_btn');
    return btn && btn.style.display !== 'none' && getComputedStyle(btn).display !== 'none';
  }, { timeout: 30000 });

  console.log('Baseline model fitted! Opening Auto-Optimize modal...');
  await ui.evaluate(() => {
    document.getElementById('prune_model_btn').click();
  });

  await ui.waitForSelector('#prune_lock_table', { timeout: 10000 });

  console.log('Selecting strategy ' + STRATEGY + '...');
  await ui.evaluate((strategy) => {
    if (window.Shiny && window.Shiny.setInputValue) window.Shiny.setInputValue('prune_strategy', strategy);
    const sel = document.getElementById('prune_strategy');
    if (sel && sel.selectize) sel.selectize.setValue(strategy);
    else if (sel) { sel.value = strategy; sel.dispatchEvent(new Event('change', { bubbles: true })); }
  }, STRATEGY);
  await new Promise(r => setTimeout(r, 800));

  await ui.evaluate(() => { document.getElementById('run_prune_explore').click(); });
  await ui.waitForSelector('#prune_candidates_table tbody tr', { timeout: 240000 });
  await new Promise(r => setTimeout(r, 1500));

  const out = await ui.evaluate(() => {
    const rows = Array.from(document.querySelectorAll('#prune_candidates_table tbody tr')).map(tr =>
      Array.from(tr.querySelectorAll('td')).map(td => td.innerText.trim()).join(' | '));
    const headers = Array.from(document.querySelectorAll('#prune_candidates_table thead th')).map(th => th.innerText.trim());
    const notes = Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.innerText.trim());
    return { title: document.querySelector('.modal-title')?.innerText.trim(), headers, rows, notes };
  });
  console.log('TITLE:', out.title);
  console.log('HEADERS:', out.headers.join(' | '));
  out.rows.forEach(r => console.log('ROW:', r));
  console.log('NOTIFICATIONS:', JSON.stringify(out.notes));
  await browser.close();
})();
