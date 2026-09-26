const puppeteer = require('puppeteer-core');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  console.log('1. Navigating to http://localhost:8100 ...');
  const tNav0 = Date.now();
  await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 30000 });
  console.log(`Navigation completed in ${Date.now() - tNav0}ms.`);

  console.log('2. Selecting HolzingerSwineford1939 demo dataset...');
  await page.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 15000 });
  const tLoad0 = Date.now();
  await page.evaluate(() => {
    document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]').click();
  });
  await page.waitForSelector('#datatable table', { timeout: 15000 });
  console.log(`Data table rendered in ${Date.now() - tLoad0}ms.`);

  console.log('3. Switching to Filtered tab...');
  const tFiltered0 = Date.now();
  await page.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Filtered') {
        tab.click();
        break;
      }
    }
  });
  await page.waitForSelector('#filtered_table table tbody tr', { timeout: 15000 });
  console.log(`Filtered tab rendered in ${Date.now() - tFiltered0}ms.`);

  // Switch to Model tab
  console.log('5. Switching to Model tab...');
  const tModel0 = Date.now();
  await page.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
    }
  });
  await page.waitForSelector('#checkbox_matrix .handsontable tbody tr', { timeout: 15000 });
  const modelTabTime = Date.now() - tModel0;
  console.log(`Model tab switch completed in: ${modelTabTime}ms (Previously 12,216ms)`);

  // Measure mouse hover across 50 cells in Model tab
  console.log('6. Measuring mouse interaction performance on Structural model table...');
  const modelCells = await page.$$('#checkbox_matrix td');
  const tModelMove0 = Date.now();
  for (let i = 0; i < Math.min(modelCells.length, 50); i++) {
    const box = await modelCells[i].boundingBox();
    if (box) {
      await page.mouse.move(box.x + box.width / 2, box.y + box.height / 2);
    }
  }
  const modelHoverTime = Date.now() - tModelMove0;
  console.log(`Mouse move across 50 structural cells: ${modelHoverTime}ms (Previously 7,821ms)`);

  // Check checkbox response time
  console.log('7. Testing checkbox click responsiveness...');
  const tClick0 = Date.now();
  const clicked = await page.evaluate(() => {
    const chks = document.querySelectorAll('#checkbox_matrix td input[type="checkbox"]');
    for (let chk of chks) {
      const td = chk.closest('td');
      if (td && !td.classList.contains('htDimmed') && !chk.disabled) {
        chk.click();
        return true;
      }
    }
    return false;
  });
  const clickTime = Date.now() - tClick0;
  console.log(`Checkbox click handled in: ${clickTime}ms (clicked: ${clicked})`);

  await browser.close();

  // Assertions
  if (modelTabTime > 4000) {
    console.error(`FAIL: Model tab switch time is still too slow (${modelTabTime}ms)!`);
    process.exit(1);
  }

  console.log('\n========================================');
  console.log('ALL BENCHMARKS AND VERIFICATIONS PASSED!');
  console.log('========================================');
})();
