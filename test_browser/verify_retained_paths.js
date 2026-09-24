const puppeteer = require('puppeteer-core');
const path = require('path');

(async () => {
  let page, browser;
  try {
    browser = await puppeteer.launch({
      executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
      headless: true,
      args: ['--no-sandbox', '--disable-setuid-sandbox']
    });
    
    page = await browser.newPage();
    page.setViewport({ width: 1280, height: 900 });

    console.log('Navigating to http://localhost:8100 ...');
    await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 30000 });

    console.log('Selecting sample dataset HolzingerSwineford1939...');
    await page.evaluate(() => {
      const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
      if (radio) {
        radio.click();
        radio.dispatchEvent(new Event('change', { bubbles: true }));
      }
    });

    await new Promise(r => setTimeout(r, 2000));

    console.log('Switching to Model tab...');
    await page.evaluate(() => {
      const tabs = document.querySelectorAll('a[data-toggle="tab"]');
      for (let tab of tabs) {
        if (tab.innerText.trim() === 'Model') {
          tab.click();
          break;
        }
      }
    });

    await page.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 20000 });
    await new Promise(r => setTimeout(r, 1000));

    console.log('Setting measurement model in Handsontable (visual: x1,x2,x3; textual: x4,x5,x6)...');
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
          [1, 13, true]  // x6
        ], 'edit');
      }
    });

    await new Promise(r => setTimeout(r, 1500));

    console.log('Setting structural path textual ~ visual in checkbox_matrix...');
    await page.evaluate(() => {
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

    await new Promise(r => setTimeout(r, 1500));

    console.log('Clicking Run / Update Model (#run_model)...');
    await page.evaluate(() => {
      const btn = document.querySelector('#run_model');
      if (btn) btn.click();
    });

    console.log('Waiting for Auto-Optimize button (#prune_model_btn) to become visible...');
    await page.waitForFunction(() => {
      const btn = document.querySelector('#prune_model_btn');
      return btn && btn.style.display !== 'none' && getComputedStyle(btn).display !== 'none';
    }, { timeout: 30000 });

    console.log('Clicking Auto-Optimize Model button (#prune_model_btn)...');
    await page.evaluate(() => {
      const btn = document.querySelector('#prune_model_btn');
      if (btn) btn.click();
    });

    console.log('Waiting for Step 1 Modal and clicking Run Optimization (#run_prune_explore)...');
    await page.waitForSelector('#run_prune_explore', { timeout: 15000 });
    await page.evaluate(() => {
      const btn = document.querySelector('#run_prune_explore');
      if (btn) btn.click();
    });

    console.log('Waiting for Candidate Catalog Modal (#prune_candidates_table)...');
    await page.waitForSelector('#prune_candidates_table', { timeout: 60000 });
    await new Promise(r => setTimeout(r, 1500));

    console.log('Extracting candidate table headers and first row...');
    const headers = await page.evaluate(() => {
      const ths = Array.from(document.querySelectorAll('#prune_candidates_table th'));
      return ths.map(th => th.innerText.trim());
    });

    const firstRowData = await page.evaluate(() => {
      const tds = Array.from(document.querySelectorAll('#prune_candidates_table tbody tr:first-child td'));
      return tds.map(td => td.innerText.trim());
    });

    console.log('\n--- VERIFICATION RESULT ---');
    console.log('Table Headers:', headers);
    console.log('First Row Data:', firstRowData);

    await page.screenshot({ path: path.join(__dirname, 'retained_paths_verification.png') });
    console.log('Screenshot saved to retained_paths_verification.png');

    await browser.close();
    if (headers.includes('Retained Paths')) {
      console.log('VERIFICATION SUCCESS: Header "Retained Paths" found!');
      process.exit(0);
    } else {
      console.error('VERIFICATION FAILED: "Retained Paths" missing from headers:', headers);
      process.exit(1);
    }
  } catch (err) {
    console.error('Error during verification:', err);
    if (page) await page.screenshot({ path: path.join(__dirname, 'debug_error.png') });
    if (browser) await browser.close();
    process.exit(1);
  }
})();
