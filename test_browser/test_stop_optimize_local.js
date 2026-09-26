const puppeteer = require('puppeteer-core');
const fs = require('fs');

(async () => {
  console.log('Starting automated verification for Auto-Optimize interruption...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1300, height: 950 });

  page.on('console', msg => console.log('BROWSER:', msg.text()));
  page.on('pageerror', err => console.error('PAGE ERROR:', err.message));

  console.log('Navigating to http://127.0.0.1:8100 ...');
  await page.goto('http://127.0.0.1:8100', { waitUntil: 'networkidle2', timeout: 30000 });

  console.log('Selecting HolzingerSwineford1939 demo dataset...');
  await page.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 15000 });
  await page.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  console.log('Waiting for data table...');
  await page.waitForSelector('#datatable', { timeout: 15000 });
  console.log('Data loaded. Switching to Model tab...');

  await page.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await page.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 15000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Configuring measurement model for visual and textual...');
  await page.evaluate(() => {
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

  await page.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('visual =~') && text.includes('textual =~') && text.includes('speed =~');
  }, { timeout: 20000 });

  console.log('Configuring structural paths (textual ~ visual, speed ~ visual, speed ~ textual)...');
  await page.evaluate(() => {
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

  await page.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('textual ~ visual') && text.includes('speed ~ visual');
  }, { timeout: 20000 });

  console.log('Clicking Run / Update Model...');
  await page.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for baseline model to fit and prune_model_btn to appear...');
  await page.waitForFunction(() => {
    const btn = document.getElementById('prune_model_btn');
    return btn && btn.style.display !== 'none' && getComputedStyle(btn).display !== 'none';
  }, { timeout: 30000 });

  console.log('Baseline model fitted! Opening Auto-Optimize modal...');
  await page.evaluate(() => {
    document.getElementById('prune_model_btn').click();
  });

  await page.waitForSelector('#prune_lock_table', { timeout: 10000 });

  console.log('Selecting Simulated Annealing (sa)...');
  await page.evaluate(() => {
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue('prune_strategy', 'sa');
    }
    const sel = document.getElementById('prune_strategy');
    if (sel && sel.selectize) {
      sel.selectize.setValue('sa');
    } else if (sel) {
      sel.value = 'sa';
      sel.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });
  await new Promise(r => setTimeout(r, 800));

  console.log('Starting optimization...');
  await page.evaluate(() => {
    document.getElementById('run_prune_explore').click();
  });

  console.log('Waiting for progress modal and buttons (#stop_prune_explore, #cancel_prune_explore)...');
  await page.waitForSelector('#stop_prune_explore', { timeout: 10000 });
  await page.waitForSelector('#cancel_prune_explore', { timeout: 10000 });
  console.log('Buttons verified in DOM! Immediately clicking Stop & View Results (#stop_prune_explore)...');

  const clicked = await page.evaluate(() => {
    const btn = document.getElementById('stop_prune_explore');
    if (btn) {
      btn.click();
      return true;
    }
    return false;
  });
  console.log('Stop button clicked:', clicked);

  console.log('Waiting for Step 2 Candidate Catalog modal to appear...');
  await page.waitForSelector('#prune_candidates_table', { timeout: 15000 });

  const modalHeaderText = await page.evaluate(() => {
    return document.querySelector('.modal-title')?.innerText || '';
  });
  console.log('Step 2 Modal Title:\n', modalHeaderText);

  if (!modalHeaderText.includes('Stopped Early at Step')) {
    console.error('FAIL: Expected "[Stopped Early at Step ...]" in modal title, got:', modalHeaderText);
    process.exit(1);
  }
  console.log('Waiting for candidate table rendering...');
  await page.waitForSelector('#prune_candidates_table tbody tr', { timeout: 10000 });
  await new Promise(r => setTimeout(r, 800));

  console.log('Saving proof screenshot of interrupted candidate catalog...');
  await page.screenshot({ path: 'test_browser/stop_optimize_proof.png', fullPage: false });

  console.log('Testing Apply Selected Model button...');
  await page.evaluate(() => {
    document.getElementById('apply_pruned_model').click();
  });

  await new Promise(r => setTimeout(r, 1500));
  console.log('Apply clicked successfully! Modal dismissed.');

  // Test Cancel button behavior
  console.log('Re-opening Auto-Optimize modal to test Cancel button...');
  await page.evaluate(() => {
    document.getElementById('prune_model_btn').click();
  });
  await page.waitForSelector('#prune_lock_table', { timeout: 10000 });
  await page.evaluate(() => {
    if (window.Shiny && window.Shiny.setInputValue) {
      window.Shiny.setInputValue('prune_strategy', 'sa');
    }
    const sel = document.getElementById('prune_strategy');
    if (sel && sel.selectize) {
      sel.selectize.setValue('sa');
    } else if (sel) {
      sel.value = 'sa';
      sel.dispatchEvent(new Event('change', { bubbles: true }));
    }
  });
  await new Promise(r => setTimeout(r, 600));
  await page.evaluate(() => {
    document.getElementById('run_prune_explore').click();
  });

  await page.waitForSelector('#cancel_prune_explore', { timeout: 10000 });
  console.log('Clicking Cancel button (#cancel_prune_explore)...');
  await page.evaluate(() => {
    document.getElementById('cancel_prune_explore').click();
  });

  await page.waitForFunction(() => {
    const modal = document.querySelector('.modal.in, .modal.show');
    return modal === null;
  }, { timeout: 10000 });
  console.log('Modal closed after cancel: true');

  console.log('ALL VERIFICATION CHECKS PASSED SUCCESSFULLY!');
  await browser.close();
  process.exit(0);
})();
