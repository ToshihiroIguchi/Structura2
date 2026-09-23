const puppeteer = require('puppeteer-core');

(async () => {
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

  console.log('Waiting 4 seconds for R to re-render checkbox_matrix with latent variables...');
  await new Promise(r => setTimeout(r, 4000));

  const matrixInfo = await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (hot) {
      return {
        data: hot.getData(),
        colHeaders: hot.getSettings().colHeaders,
        rowHeaders: hot.getSettings().rowHeaders
      };
    }
    return null;
  });

  console.log('Updated checkbox_matrix info:\n', JSON.stringify(matrixInfo, null, 2));

  // Find row index for 'textual' and col index for 'visual' dynamically!
  const targetIndices = await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (!hot) return null;
    const data = hot.getData();
    const headers = hot.getSettings().colHeaders;

    let rowIdx = -1;
    for (let r = 0; r < data.length; r++) {
      if (data[r][0] === 'textual') { rowIdx = r; break; }
    }

    let colIdx = -1;
    for (let c = 0; c < headers.length; c++) {
      if (headers[c] === 'visual') { colIdx = c; break; }
    }

    if (rowIdx >= 0 && colIdx >= 0) {
      hot.setDataAtCell([[rowIdx, colIdx, true]], 'edit');
    }

    return { rowIdx, colIdx };
  });

  console.log('Dynamically found indices for textual ~ visual:', targetIndices);

  await new Promise(r => setTimeout(r, 2000));

  const syntaxStr = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Generated lavaan model syntax after dynamic matrix edit:\n', syntaxStr);

  await browser.close();
})();
