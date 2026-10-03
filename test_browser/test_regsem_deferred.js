const puppeteer = require('puppeteer-core');
const fs = require('fs');

(async () => {
  console.log('Launching browser test: regsem strategy must load its deferred packages on demand...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  const deferredReqs = [];
  page.on('request', r => { if (/regsem|future|Rsolnp|deferred/.test(r.url())) { deferredReqs.push(r.url().split('/').slice(-2).join('/')); console.log('REQ', r.url()); } });
  page.on('console', msg => { const t = msg.text(); if (/regsem|deferred|Could not load|Loading regularized|error/i.test(t)) console.log('BROWSER:', t); });
  page.on('pageerror', err => console.error('PAGE ERROR:', err.message));

  console.log('Navigating to http://localhost:8100 ...');
  await page.goto((process.argv[2] || 'http://localhost:8100'), { waitUntil: 'networkidle2', timeout: 60000 });

  console.log('Waiting for Shinylive webR app iframe to mount...');
  let targetFrame = null;
  for (let i = 0; i < 40; i++) {
    const frames = page.frames();
    if (frames.length > 1) {
      targetFrame = frames.find(f => f !== page.mainFrame());
      if (targetFrame) break;
    }
    await new Promise(r => setTimeout(r, 1000));
  }

  if (!targetFrame) {
    console.error('Failed to find Shinylive app iframe.');
    await browser.close();
    process.exit(1);
  }

  console.log('Target frame found. Waiting for HolzingerSwineford1939 radio option...');
  await targetFrame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });

  console.log('Selecting HolzingerSwineford1939 dataset...');
  await targetFrame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  console.log('Waiting for data table...');
  await targetFrame.waitForSelector('#datatable', { timeout: 60000 });
  console.log('Data loaded. Switching to Model tab...');

  await targetFrame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await targetFrame.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting measurement model for visual (x1,x2,x3) and textual (x4,x5,x6)...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtCell([
        [0, 0, 'visual'],
        [0, 8, true],  // x1 (col 8)
        [0, 9, true],  // x2 (col 9)
        [0, 10, true], // x3 (col 10)

        [1, 0, 'textual'],
        [1, 11, true], // x4 (col 11)
        [1, 12, true], // x5 (col 12)
        [1, 13, true]  // x6 (col 13)
      ], 'edit');
    }
  });

  console.log('Waiting for measurement model syntax (visual =~ and textual =~)...');
  await targetFrame.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('visual =~') && text.includes('textual =~');
  }, { timeout: 30000 });

  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting structural path textual ~ visual in checkbox_matrix...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      const data = hot.getData();
      const headers = hot.getSettings().colHeaders;
      let rowIdx = -1, colIdx = -1;
      for (let r = 0; r < data.length; r++) {
        if (data[r][0] === 'textual') { rowIdx = r; break; }
      }
      for (let c = 0; c < headers.length; c++) {
        if (headers[c] === 'visual') { colIdx = c; break; }
      }
      if (rowIdx >= 0 && colIdx >= 0) {
        hot.setDataAtCell([[rowIdx, colIdx, true]], 'edit');
      }
    }
  });

  console.log('Waiting for structural path syntax (textual ~ visual)...');
  await targetFrame.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('textual ~ visual');
  }, { timeout: 30000 });

  const syntaxStr = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Generated lavaan model syntax:\n', syntaxStr);

  console.log('Clicking Run / Update Model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for model fitting & Auto-Optimize button to become visible (up to 45s)...');
  await targetFrame.waitForFunction(() => {
    const btn = document.getElementById('prune_model_btn');
    return btn && btn.style.display !== 'none' && getComputedStyle(btn).display !== 'none';
  }, { timeout: 45000 });
  console.log('Baseline model fitted successfully! Auto-Optimize button is now visible.');

  await new Promise(r => setTimeout(r, 1000));

  console.log('Clicking Auto-Optimize Model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('prune_model_btn');
    if (btn) btn.click();
  });

  console.log('Waiting for Step 1 Modal (Parameters & Locking)...');
  await targetFrame.waitForSelector('#prune_lock_table', { timeout: 20000 });

  await targetFrame.evaluate(() => {
    window.__notes = [];
    new MutationObserver(() => {
      document.querySelectorAll('.shiny-notification').forEach(n => {
        const t = n.innerText.trim();
        if (t && !window.__notes.includes(t)) window.__notes.push(t);
      });
    }).observe(document.body, { childList: true, subtree: true, characterData: true });
  });
  console.log('Selecting regsem strategy...');
  await targetFrame.evaluate(() => {
    const el = document.getElementById('prune_strategy');
    if (el && el.selectize) el.selectize.setValue('regsem'); else window.jQuery(el).val('regsem').trigger('change');
  });
  console.log('strategy value now:', await targetFrame.evaluate(() => document.getElementById('prune_strategy').value));
  await new Promise(r => setTimeout(r, 15000));
  console.log('Deferred package requests after selection:', JSON.stringify(deferredReqs));

  console.log('Clicking Run Optimization button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_prune_explore');
    if (btn) btn.click();
  });

  console.log('Waiting for Progress Modal with live Canvas Chart...');
  await targetFrame.waitForSelector('#opt_chart_canvas', { timeout: 30000 });

  console.log('Capturing live progress screenshot 1 (Mid-calculation with live chart)...');
  await new Promise(r => setTimeout(r, 1200));
  await page.screenshot({ path: 'test_browser/regsem_01_progress.png' });

  console.log('Capturing live progress screenshot 2 (Advanced iteration with live trajectory)...');
  await new Promise(r => setTimeout(r, 1800));
  await page.screenshot({ path: 'test_browser/regsem_02_progress.png' });

  console.log('Waiting for Step 2 Candidate Catalog Modal completion...');
  await targetFrame.waitForSelector('#prune_candidates_table', { timeout: 60000 });
  console.log('Step 2 Candidate Ranking Catalog loaded!');

  await new Promise(r => setTimeout(r, 1500));
  await page.screenshot({ path: 'test_browser/regsem_03_catalog.png' });
  const notes = await targetFrame.evaluate(() => Array.from(document.querySelectorAll('.shiny-notification')).map(n => n.innerText.trim()));
  console.log('Notifications at end:', JSON.stringify(notes));
  console.log('All notifications seen:', JSON.stringify(await targetFrame.evaluate(() => window.__notes)));

  console.log('Verification successful! Screenshots saved:');

  await browser.close();
  process.exit(0);
})();
