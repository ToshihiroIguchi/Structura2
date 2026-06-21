const puppeteer = require('puppeteer-core');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  console.log('Navigating to http://localhost:8080 ...');
  try {
    await page.goto('http://localhost:8080', { waitUntil: 'networkidle2', timeout: 30000 });
  } catch (err) {
    console.error('Failed to navigate to app. Make sure the local server is running on 8080.', err);
    await browser.close();
    process.exit(1);
  }
  
  console.log('Selecting HolzingerSwineford1939 demo dataset...');
  try {
    await page.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 15000 });
    await page.evaluate(() => {
      const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
      if (radio) radio.click();
    });
  } catch (err) {
    console.error('HolzingerSwineford1939 demo option not found.', err);
    await page.screenshot({ path: 'test_browser/error_local_modal.png' });
    await browser.close();
    process.exit(1);
  }
  
  console.log('Waiting for data table to load...');
  try {
    await page.waitForSelector('#datatable', { timeout: 15000 });
    console.log('Data loaded successfully.');
  } catch (err) {
    console.error('Data table failed to load.');
    await page.screenshot({ path: 'test_browser/error_local_dataload.png' });
    await browser.close();
    process.exit(1);
  }
  
  console.log('Switching to Model tab...');
  await page.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
    }
  });

  await page.waitForSelector('#input_table', { timeout: 10000 });
  console.log('Model tab loaded. Waiting 3s for cells to render...');
  await new Promise(resolve => setTimeout(resolve, 3000));
  
  console.log('Checking a path in Measurement Model to define the model...');
  
  const checked = await page.evaluate(() => {
    const tds = document.querySelectorAll('#input_table td');
    let clickedCount = 0;
    for (let td of tds) {
      const checkbox = td.querySelector('input[type="checkbox"]');
      if (checkbox && !td.classList.contains('htDimmed') && !checkbox.disabled) {
        checkbox.click();
        clickedCount++;
        if (clickedCount >= 3) break;
      }
    }
    return clickedCount > 0;
  });
  console.log('Checked measurement paths:', checked);
  
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  console.log('Clicking Run Model button...');
  try {
    await page.evaluate(() => {
      const btn = document.getElementById('run_model');
      if (btn) btn.click();
    });
  } catch (err) {
    console.error('Failed to click Run Model button.', err);
    await browser.close();
    process.exit(1);
  }
  
  console.log('Waiting for path diagram rendering...');
  try {
    await page.waitForSelector('#sem_plot_container svg', { timeout: 20000 });
    console.log('Path diagram SVG rendered successfully via @hpcc-js/wasm!');
  } catch (err) {
    console.error('Path diagram SVG failed to render.');
    const containerHtml = await page.evaluate(() => document.getElementById('sem_plot_container').innerHTML);
    console.log('Container HTML:', containerHtml);
    await page.screenshot({ path: 'test_browser/error_local_diagram.png' });
    await browser.close();
    process.exit(1);
  }

  console.log('Changing layout to circular (circo)...');
  await page.select('#layout_style', 'circo');
  await new Promise(resolve => setTimeout(resolve, 2000));
  
  const hasCircoSvg = await page.evaluate(() => {
    const svg = document.querySelector('#sem_plot_container svg');
    return svg !== null;
  });
  console.log('Circular layout rendered SVG:', hasCircoSvg);

  console.log('Taking test screen shot...');
  await page.screenshot({ path: 'test_browser/test_local_screenshot.png', fullPage: true });
  console.log('Test completed successfully. Screenshot saved to test_browser/test_local_screenshot.png');

  await browser.close();
})();
