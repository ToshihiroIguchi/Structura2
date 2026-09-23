const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing hot.selectCell + setDataAtCell + deselectCell user emulation...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  await page.goto('http://localhost:8080', { waitUntil: 'networkidle2', timeout: 60000 });

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

  console.log('Setting input_table via selectCell + setDataAtCell + deselectCell...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    const inst = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container) : null;
    const hot = inst ? inst.hot : null;
    if (hot) {
      // Set row 0: Latent = 'visual', x1, x2, x3 = true
      hot.selectCell(0, 0);
      hot.setDataAtCell(0, 0, 'visual');
      hot.setDataAtCell(0, 8, true);
      hot.setDataAtCell(0, 9, true);
      hot.setDataAtCell(0, 10, true);
      hot.deselectCell();
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  const syntax1 = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Syntax after input_table deselectCell:\n', syntax1);

  await targetFrame.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('Setting checkbox_matrix via selectCell + setDataAtCell + deselectCell...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    const inst = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container) : null;
    const hot = inst ? inst.hot : null;
    if (hot) {
      hot.selectCell(0, 2);
      hot.setDataAtCell(0, 2, true);
      hot.deselectCell();
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  const syntax2 = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Syntax after checkbox_matrix deselectCell:\n', syntax2);

  await browser.close();
})();
