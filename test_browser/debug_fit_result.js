const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Debugging fit_model_safe output and prune_model_btn visibility...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  page.on('console', msg => console.log('BROWSER:', msg.text()));

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
    document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]').click();
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

  console.log('Setting input_table latents and measurement indicators...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (hot) {
      hot.setDataAtCell([
        [0, 0, 'visual'],
        [0, 8, true], [0, 9, true], [0, 10, true],
        [1, 0, 'textual'],
        [1, 11, true], [1, 12, true], [1, 13, true]
      ], 'edit');
    }
  });

  console.log('Waiting for measurement model syntax...');
  await targetFrame.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('visual =~') && text.includes('textual =~');
  }, { timeout: 30000 });

  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting structural path in checkbox_matrix...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot) return;
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
  });

  console.log('Waiting for structural path syntax...');
  await targetFrame.waitForFunction(() => {
    const text = document.getElementById('lavaan_model')?.innerText || '';
    return text.includes('textual ~ visual');
  }, { timeout: 30000 });

  console.log('Clicking Run / Update Model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting 5 seconds after Run / Update Model...');
  await new Promise(r => setTimeout(r, 5000));

  const status = await targetFrame.evaluate(() => {
    const btn = document.getElementById('prune_model_btn');
    const alert = document.getElementById('fit_alert')?.innerText;
    const syntax = document.getElementById('lavaan_model')?.innerText;
    return {
      btnDisplay: btn ? btn.style.display : 'not_found',
      computedDisplay: btn ? getComputedStyle(btn).display : 'not_found',
      fitAlert: alert,
      syntax: syntax
    };
  });

  console.log('App Status after Run Model:', JSON.stringify(status, null, 2));

  await browser.close();
})();
