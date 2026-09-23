const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Inspecting HTMLWidgets.widgets registration...');
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

  const widgetSrc = await targetFrame.evaluate(() => {
    if (window.HTMLWidgets && window.HTMLWidgets.widgets) {
      const rhWidget = window.HTMLWidgets.widgets.find(w => w.name === 'rhandsontable');
      if (rhWidget) {
        return {
          name: rhWidget.name,
          type: rhWidget.type,
          renderValueSrc: rhWidget.renderValue ? rhWidget.renderValue.toString() : null
        };
      }
    }
    return null;
  });

  console.log('rhandsontable widget renderValue source:\n', widgetSrc?.renderValueSrc);
  await browser.close();
})();
