const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing hot.setDataAtCell with edit source...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 60000 });

  let targetFrame = null;
  for (let i = 0; i < 40; i++) {
    const frames = page.frames();
    if (frames.length > 1) {
      targetFrame = frames.find(f => f !== page.mainFrame());
      if (targetFrame) break;
    }
    await new Promise(r => setTimeout(r, 1000));
  }

  await targetFrame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });
  await targetFrame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  await targetFrame.waitForSelector('#datatable', { timeout: 60000 });
  await targetFrame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await targetFrame.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting input_table via hot.setDataAtCell(..., "edit")...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      // Pass 'edit' as 4th argument so Handsontable triggers afterChange
      hot.setDataAtCell([
        [0, 0, 'visual'],
        [0, 3, true],
        [0, 4, true],
        [0, 5, true]
      ], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  let syntax1 = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Syntax after input_table edits:\n', syntax1);

  console.log('Waiting for #checkbox_matrix...');
  await targetFrame.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting checkbox_matrix via hot.setDataAtCell(..., "edit")...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtCell([[0, 2, true]], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  let syntax2 = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Syntax after checkbox_matrix edit:\n', syntax2);

  console.log('Clicking run_model...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  await new Promise(r => setTimeout(r, 4000));

  const pruneBtnDisplay = await targetFrame.evaluate(() => {
    const btn = document.getElementById('prune_model_btn');
    return btn ? getComputedStyle(btn).display : 'none';
  });
  console.log('Prune Model Button Display:', pruneBtnDisplay);

  await page.screenshot({ path: 'test_browser/debug_edit_source.png', fullPage: true });
  await browser.close();
})();
