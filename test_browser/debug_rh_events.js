const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing rhandsontable event triggers...');
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

  console.log('Testing hot.setDataAtCell + deselectCell / mouse events...');
  const res = await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    const inst = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container) : null;
    const hot = inst ? inst.hot : null;
    if (hot) {
      // 1. Set Latent name to visual
      hot.setDataAtCell(0, 0, 'visual');
      // 2. Set x1, x2, x3 (cols 8, 9, 10) to true
      hot.setDataAtCell(0, 8, true);
      hot.setDataAtCell(0, 9, true);
      hot.setDataAtCell(0, 10, true);

      // Trigger Shiny input change for rhandsontable
      if (window.Shiny && window.Shiny.setInputValue) {
        // rhandsontable Shiny binding input name is the container ID
        // Let's check Shiny.setInputValue
        const rData = {
          data: hot.getData(),
          params: { rClass: 'data.frame' }
        };
        window.Shiny.setInputValue('input_table', rData, { priority: 'event' });
      }
    }
    return document.getElementById('lavaan_model')?.innerText;
  });

  console.log('Lavaan model text after Shiny.setInputValue:\n', res);

  await new Promise(r => setTimeout(r, 2000));
  const res2 = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Lavaan model text after 2s:\n', res2);

  await browser.close();
})();
