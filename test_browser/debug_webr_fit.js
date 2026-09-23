const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing 2-latent SEM model for prune_model_btn visibility...');
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

  console.log('Setting up 2 latent variables in input_table via hot.setDataAtRowProp...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      // Row 0: LV1 =~ x1 + x2 + x3
      hot.setDataAtRowProp([
        [0, 'Latent', 'LV1'],
        [0, 'x1', true],
        [0, 'x2', true],
        [0, 'x3', true]
      ], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 1500));

  console.log('Adding row 2 in input_table for LV2...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('add_row');
    if (btn) btn.click();
  });

  await new Promise(r => setTimeout(r, 1500));

  console.log('Setting LV2 =~ x4 + x5 + x6 in input_table...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtRowProp([
        [1, 'Latent', 'LV2'],
        [1, 'x4', true],
        [1, 'x5', true],
        [1, 'x6', true]
      ], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  console.log('Setting structural path LV2 ~ LV1 in checkbox_matrix...');
  await targetFrame.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot && window.jQuery) hot = window.jQuery(container).data('hot');
    if (hot) {
      hot.setDataAtRowProp([[1, 'LV1', true]], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 1500));

  const syntax = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Generated 2-latent SEM model syntax:\n', syntax);

  console.log('Clicking run_model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Monitoring prune_model_btn visibility...');
  for (let s = 1; s <= 30; s++) {
    await new Promise(r => setTimeout(r, 1000));
    const btnState = await targetFrame.evaluate(() => {
      const btn = document.getElementById('prune_model_btn');
      return btn ? { display: getComputedStyle(btn).display, visible: btn.offsetWidth > 0 && btn.offsetHeight > 0 } : null;
    });
    console.log(`Sec ${s}: prune_model_btn state =`, JSON.stringify(btnState));
    if (btnState && btnState.display !== 'none') {
      console.log(`SUCCESS! prune_model_btn became visible at second ${s}!`);
      await page.screenshot({ path: 'test_browser/evidence_2latent_prune_btn.png', fullPage: true });
      break;
    }
  }

  await browser.close();
})();
