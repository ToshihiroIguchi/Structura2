const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Inspecting HTMLWidgets rhandsontable trigger methods...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

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

  const hotFuncs = await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    const inst = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container) : null;
    const hot = inst ? inst.hot : null;
    
    // Check rhandsontable plugin or settings
    const settings = hot ? hot.getSettings() : {};
    return {
      settingsKeys: Object.keys(settings),
      hasAfterChange: typeof settings.afterChange === 'function',
      hasAfterCellMetaChange: typeof settings.afterCellMetaChange === 'function'
    };
  });

  console.log('HOT Funcs:', JSON.stringify(hotFuncs, null, 2));

  // Test calling settings.afterChange
  console.log('Testing calling hot.getSettings().afterChange...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    const inst = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container) : null;
    const hot = inst ? inst.hot : null;
    if (hot) {
      const changes = [
        [0, 'Latent', '', 'visual'],
        [0, 'x1', false, true],
        [0, 'x2', false, true],
        [0, 'x3', false, true]
      ];
      hot.setDataAtRowProp([
        [0, 'Latent', 'visual'],
        [0, 'x1', true],
        [0, 'x2', true],
        [0, 'x3', true]
      ], 'edit');
      
      const settings = hot.getSettings();
      if (typeof settings.afterChange === 'function') {
        settings.afterChange.call(hot, changes, 'edit');
      }
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  const syntax = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Syntax after calling afterChange explicitly:\n', syntax);

  await browser.close();
})();
