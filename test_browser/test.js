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

  console.log('Navigating to http://localhost:8000 ...');
  try {
    await page.goto('http://localhost:8000', { waitUntil: 'networkidle2', timeout: 60000 });
  } catch (err) {
    console.error('Failed to navigate to app. Make sure the local server is running.', err);
    await browser.close();
    process.exit(1);
  }
  
  console.log('Waiting for shinylive iframe to load...');
  let frame;
  try {
    await page.waitForSelector('iframe', { timeout: 30000 });
    const iframeElement = await page.$('iframe');
    frame = await iframeElement.contentFrame();
    if (!frame) throw new Error('Could not get contentFrame from iframe');
  } catch (err) {
    console.error('shinylive iframe not found or failed to load.', err);
    await page.screenshot({ path: 'C:\\Users\\toshi\\python\\Structura2\\test_browser\\error_iframe.png' });
    await browser.close();
    process.exit(1);
  }

  console.log('Waiting for Load Data modal inside iframe...');
  try {
    await frame.waitForSelector('#sample_ds', { timeout: 90000 });
  } catch (err) {
    console.error('Modal #sample_ds not found inside iframe.', err);
    const bodyHtml = await page.evaluate(() => document.body.innerHTML);
    const frameHtml = await frame.evaluate(() => document.body.innerHTML);
    console.log('Parent HTML length:', bodyHtml.length);
    console.log('Iframe HTML length:', frameHtml.length);
    await page.screenshot({ path: 'C:\\Users\\toshi\\python\\Structura2\\test_browser\\error_startup.png' });
    await browser.close();
    process.exit(1);
  }
  
  console.log('Selecting HolzingerSwineford1939 demo dataset...');
  await frame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) {
      radio.click();
    } else {
      throw new Error('Radio button for HolzingerSwineford1939 not found');
    }
  });
  
  console.log('Waiting for modal to dismiss and data to load...');
  try {
    await frame.waitForSelector('#datatable', { timeout: 30000 });
    console.log('Data loaded successfully.');
  } catch (err) {
    console.error('Data table failed to load inside iframe.');
    await page.screenshot({ path: 'C:\\Users\\toshi\\python\\Structura2\\test_browser\\error_dataload.png' });
    await browser.close();
    process.exit(1);
  }

  // Check if fit alert box is visible (should be hidden on startup now)
  const alertVisible = await frame.evaluate(() => {
    const box = document.getElementById('fit_alert_box');
    return box && box.style.display !== 'none';
  });
  console.log('Startup fit alert box visible (expected: false):', alertVisible);

  console.log('Switching to Model tab...');
  await frame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
    }
  });

  await frame.waitForSelector('#input_table', { timeout: 10000 });
  await frame.waitForSelector('#checkbox_matrix', { timeout: 10000 });
  console.log('Model tab tables selector found. Waiting 2.5s for cells to render...');
  await new Promise(resolve => setTimeout(resolve, 2500));

  console.log('Checking a path in Structural Model (checkbox_matrix)...');
  const clicked = await frame.evaluate(() => {
    const tds = document.querySelectorAll('#checkbox_matrix td');
    console.log('Total td cells found in checkbox_matrix:', tds.length);
    for (let td of tds) {
      const checkbox = td.querySelector('input[type="checkbox"]');
      if (checkbox && !td.classList.contains('htDimmed') && !checkbox.disabled) {
        checkbox.click();
        return true;
      }
    }
    return false;
  });
  console.log('Clicked a valid checkbox in structural model:', clicked);

  // Wait a bit for state to save to reactiveVal
  await new Promise(resolve => setTimeout(resolve, 1500));

  console.log('Switching to Data tab to trigger re-render of Model tab...');
  await frame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Data') {
        tab.click();
        break;
      }
    }
  });
  await new Promise(resolve => setTimeout(resolve, 1000));

  console.log('Switching back to Model tab...');
  await frame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
    }
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  // Check if checkbox is still checked
  const checkboxStates = await frame.evaluate(() => {
    const tds = document.querySelectorAll('#checkbox_matrix td');
    const states = [];
    for (let td of tds) {
      const checkbox = td.querySelector('input[type="checkbox"]');
      if (checkbox && !td.classList.contains('htDimmed')) {
        states.push(checkbox.checked);
      }
    }
    return states;
  });
  
  const isAnyChecked = checkboxStates.some(state => state === true);
  console.log('Are there checked paths in structural model after switching tabs (expected: true):', isAnyChecked);

  console.log('Taking final screenshot...');
  await page.screenshot({ path: 'C:\\Users\\toshi\\python\\Structura2\\test_browser\\screenshot.png', fullPage: true });
  console.log('Screenshot saved to test_browser/screenshot.png');

  await browser.close();
  console.log('Test completed.');
})();
