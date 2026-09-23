const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Debugging run_model execution...');
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

  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = null;
    if (window.HTMLWidgets) hot = window.HTMLWidgets.getInstance(container)?.hot;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtCell(0, 0, 'visual');
      hot.setDataAtCell(0, 3, true);
      hot.setDataAtCell(0, 4, true);
      hot.setDataAtCell(0, 5, true);
      hot.setDataAtCell(1, 0, 'textual');
      hot.setDataAtCell(1, 6, true);
      hot.setDataAtCell(1, 7, true);
      hot.setDataAtCell(1, 8, true);
    }
  });

  await new Promise(r => setTimeout(r, 1500));
  await targetFrame.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = null;
    if (window.HTMLWidgets) hot = window.HTMLWidgets.getInstance(container)?.hot;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      for (let c = 2; c < hot.countCols(); c++) {
        const prop = hot.colToProp(c);
        if (prop !== 'Dependent' && prop !== 'Operator') {
          hot.setDataAtCell(0, c, true);
          break;
        }
      }
    }
  });

  await new Promise(r => setTimeout(r, 1000));

  console.log('Clicking run_model...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting 10s after run_model click...');
  await new Promise(r => setTimeout(r, 10000));

  const status = await targetFrame.evaluate(() => {
    const pruneBtn = document.getElementById('prune_model_btn');
    const errBox = document.getElementById('latent_error_box');
    const runBtn = document.getElementById('run_model');
    const lavaanSyntax = document.getElementById('lavaan_model')?.innerText;
    return {
      pruneBtnDisplay: pruneBtn ? getComputedStyle(pruneBtn).display : 'null',
      errBoxDisplay: errBox ? getComputedStyle(errBox).display : 'null',
      errBoxText: errBox ? errBox.innerText : '',
      runBtnDisabled: runBtn ? runBtn.disabled : 'null',
      lavaanSyntax: lavaanSyntax || ''
    };
  });

  console.log('Page status:', JSON.stringify(status, null, 2));

  await page.screenshot({ path: 'test_browser/debug_run_model_shot.png', fullPage: true });
  await browser.close();
})();
