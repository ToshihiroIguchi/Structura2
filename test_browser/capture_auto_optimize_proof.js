const puppeteer = require('puppeteer-core');
const path = require('path');
const fs = require('fs');

(async () => {
  console.log('Launching Edge browser for proof screenshot...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    viewport: { width: 1400, height: 950 },
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  console.log('Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded', timeout: 60000 });

  console.log('Waiting for Shinylive iframe to mount...');
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

  console.log('Target iframe found! Waiting for HolzingerSwineford1939 demo dataset option...');
  await targetFrame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });

  console.log('Selecting HolzingerSwineford1939 dataset...');
  await targetFrame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  console.log('Waiting for data table (#datatable)...');
  await targetFrame.waitForSelector('#datatable', { timeout: 60000 });
  console.log('Data loaded successfully. Switching to Model tab...');

  await targetFrame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
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

  console.log('Waiting for model syntax generation...');
  await targetFrame.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('visual =~') && text.includes('textual =~');
  }, { timeout: 30000 });

  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting structural path textual ~ visual...');
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

  await new Promise(r => setTimeout(r, 1000));

  console.log('Clicking Run / Update Model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for baseline model fit and Auto-Optimize button visibility...');
  await targetFrame.waitForFunction(() => {
    const btn = document.getElementById('prune_model_btn');
    return btn && btn.style.display !== 'none' && getComputedStyle(btn).display !== 'none';
  }, { timeout: 45000 });

  console.log('Clicking Auto-Optimize Model button (#prune_model_btn)...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('prune_model_btn');
    if (btn) btn.click();
  });

  console.log('Waiting for Auto-Optimize Modal (#prune_strategy)...');
  await targetFrame.waitForSelector('#prune_strategy', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1500));

  console.log('Expanding Advanced Hyper-Parameters details panel & scrolling modal...');
  await targetFrame.evaluate(() => {
    const details = document.querySelector('.shiny-modal details');
    if (details) details.open = true;
    const modal = document.querySelector('.modal');
    if (modal) modal.scrollTop = modal.scrollHeight;
    
    const elem = document.getElementById('prune_strategy');
    if (elem) elem.scrollIntoView({ behavior: 'instant', block: 'center' });
  });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Clicking Selectize input to open dropdown menu...');
  await targetFrame.evaluate(() => {
    const selectizeInput = document.querySelector('#prune_strategy + .selectize-control .selectize-input');
    if (selectizeInput) {
      selectizeInput.click();
    } else {
      const container = document.querySelector('.shiny-input-container:has(#prune_strategy) .selectize-input');
      if (container) container.click();
    }
  });
  await new Promise(r => setTimeout(r, 1000));

  const selectizeOptions = await targetFrame.evaluate(() => {
    const options = document.querySelectorAll('.selectize-dropdown .option');
    return Array.from(options).map(o => ({
      text: o.innerText.trim(),
      dataValue: o.getAttribute('data-value')
    }));
  });

  console.log('====================================================');
  console.log('=== REAL RENDERED SELECTIZE DROPDOWN OPTIONS ===');
  console.log(JSON.stringify(selectizeOptions, null, 2));
  console.log('====================================================');

  const proofPath = path.join(__dirname, 'proof_auto_optimize_modal.png');
  await page.screenshot({ path: proofPath, fullPage: false });
  console.log(`Proof screenshot successfully saved to: ${proofPath}`);

  // Copy to artifact directory if available
  const artifactDir = 'C:\\Users\\toshi\\.gemini\\antigravity\\brain\\3ceef936-6870-4fa3-823a-6fdaa38c38f1';
  if (fs.existsSync(artifactDir)) {
    const artifactPath = path.join(artifactDir, 'proof_auto_optimize_modal.png');
    fs.copyFileSync(proofPath, artifactPath);
    console.log(`Proof screenshot copied to artifact dir: ${artifactPath}`);
  }

  await browser.close();
  console.log('Browser capture process finished successfully!');
  process.exit(0);
})();
