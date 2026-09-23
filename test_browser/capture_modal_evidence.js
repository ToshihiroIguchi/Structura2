const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Launching browser...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    viewport: { width: 1280, height: 900 },
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));
  
  console.log('Navigating to http://localhost:8080 ...');
  await page.goto('http://localhost:8080', { waitUntil: 'domcontentloaded', timeout: 60000 });
  
  console.log('Waiting for Load Data modal / sample_ds radio (up to 90s for WebR preload)...');
  await page.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });
  console.log('Found sample_ds! Selecting HolzingerSwineford1939...');
  await page.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });
  
  console.log('Waiting for data table to load...');
  await page.waitForSelector('#datatable', { timeout: 30000 });
  console.log('Data table loaded.');
  
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

  await page.waitForSelector('#input_table', { timeout: 20000 });
  await new Promise(resolve => setTimeout(resolve, 2000));
  
  console.log('Setting up indicators in measurement model...');
  await page.evaluate(() => {
    const tds = document.querySelectorAll('#input_table td');
    let count = 0;
    for (let td of tds) {
      const checkbox = td.querySelector('input[type="checkbox"]');
      if (checkbox && !td.classList.contains('htDimmed') && !checkbox.disabled) {
        if (!checkbox.checked) checkbox.click();
        count++;
        if (count >= 6) break;
      }
    }
  });
  
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  console.log('Fitting baseline model...');
  await page.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });
  
  await new Promise(resolve => setTimeout(resolve, 3000));
  
  console.log('Clicking Auto-Optimize Model button...');
  await page.evaluate(() => {
    const btn = document.getElementById('prune_model_btn');
    if (btn) btn.click();
  });
  
  console.log('Waiting for Auto-Optimize Step 1 Modal to appear...');
  await page.waitForSelector('#prune_strategy', { timeout: 20000 });
  await new Promise(resolve => setTimeout(resolve, 1500));
  
  console.log('Expanding Advanced Hyper-Parameters section...');
  await page.evaluate(() => {
    const details = document.querySelector('.shiny-modal details');
    if (details) details.open = true;
  });
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  // Extract all options in prune_strategy dropdown
  const dropdownOptions = await page.evaluate(() => {
    const select = document.getElementById('prune_strategy');
    if (!select) return [];
    return Array.from(select.options).map(opt => ({ text: opt.text, value: opt.value }));
  });
  
  console.log('Captured prune_strategy options:', JSON.stringify(dropdownOptions, null, 2));
  
  console.log('Taking screenshot of Auto-Optimize Modal evidence...');
  await page.screenshot({ path: 'test_browser/evidence_auto_optimize_modal.png', fullPage: false });
  
  await browser.close();
  console.log('SUCCESS! Screenshot saved to test_browser/evidence_auto_optimize_modal.png');
})();

