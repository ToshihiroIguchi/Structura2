const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing DOM click interaction for Structura2...');
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

  // Switch to Model tab
  await targetFrame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await targetFrame.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  // Set Latent names first via setDataAtCell, then trigger rhandsontable
  console.log('Setting latent names in input_table...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (hot) {
      hot.setDataAtCell([[0, 0, 'visual'], [1, 0, 'textual']], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  console.log('Clicking measurement checkboxes in input_table...');
  // Click checkboxes in row 0 (col index for x1, x2, x3 is 8, 9, 10 -> in DOM tr[1] td[9], td[10], td[11] since col 0 is row header or first td)
  // Let's inspect tr and td structure of input_table
  const cellStructure = await targetFrame.evaluate(() => {
    const rows = document.querySelectorAll('#input_table tbody tr');
    let info = [];
    rows.forEach((tr, rIdx) => {
      const tds = tr.querySelectorAll('td');
      tds.forEach((td, cIdx) => {
        const cb = td.querySelector('input[type="checkbox"]');
        if (cb) {
          info.push({ row: rIdx, col: cIdx, checked: cb.checked });
        }
      });
    });
    return info;
  });
  console.log('Input table checkboxes found:', cellStructure.length, cellStructure.slice(0, 10));

  // Let's click checkboxes directly using Handsontable setDataAtCell + dispatching change event or setDataAtCell with sleep
  await targetFrame.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (hot) {
      // visual: x1(8), x2(9), x3(10)
      // textual: x4(11), x5(12), x6(13)
      hot.setDataAtCell([
        [0, 8, true], [0, 9, true], [0, 10, true],
        [1, 11, true], [1, 12, true], [1, 13, true]
      ], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  console.log('Checking checkbox_matrix state...');
  await targetFrame.waitForSelector('#checkbox_matrix', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1500));

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
  console.log('checkbox_matrix info:', JSON.stringify(matrixInfo));

  // Set structural path textual ~ visual
  // In checkbox_matrix, rows are target variables, cols are predictor variables
  // Row 0 is visual, Row 1 is textual. Col 0 is latent_name, Col 1 is visual, Col 2 is textual.
  // So row 1 ('textual'), col 1 ('visual') should be checked!
  console.log('Setting structural path in checkbox_matrix (row 1, col 1 = textual ~ visual)...');
  await targetFrame.evaluate(() => {
    const container = document.getElementById('checkbox_matrix');
    let hot = window.HTMLWidgets ? window.HTMLWidgets.getInstance(container)?.hot : null;
    if (hot) {
      // row 1 (textual), col 1 (visual) -> true
      // or row 0 (visual), col 2 (textual) -> depending on order
      // Let's set both [0, 2, true] and [1, 1, true] to be safe
      hot.setDataAtCell([[0, 2, true], [1, 1, true]], 'edit');
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  const syntaxStr = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Generated lavaan model syntax after matrix edit:\n', syntaxStr);

  await browser.close();
})();
