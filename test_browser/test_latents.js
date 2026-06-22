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
  await page.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
  await page.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });
  
  await page.waitForSelector('#datatable');
  console.log('Data loaded. Switching to Model tab...');
  
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
  console.log('Waiting 3s for cells to render...');
  await new Promise(resolve => setTimeout(resolve, 3000));

  // --- Test Case 1: Single Indicator Latent Variable ---
  console.log('\n--- Running Test Case 1: Latent variable with single indicator ---');
  
  // Select only x1 for LatentVariable1
  await page.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = null;
    if (window.HTMLWidgets) {
      const inst = window.HTMLWidgets.getInstance(container);
      if (inst) hot = inst.hot;
    }
    if (!hot && window.jQuery) {
      hot = window.jQuery(container).data('hot');
    }
    if (hot) {
      // Clear all checkboxes first for the first row, then check only one
      // The columns are: Latent, Indicator, Operator, x1, x2, x3, x4, x5, x6, x7, x8, x9
      // Col 0: Latent, Col 1: Indicator, Col 2: Operator, Col 3: x1, Col 4: x2...
      hot.setDataAtCell(0, 0, 'LV1');
      for (let c = 3; c < hot.countCols(); c++) {
        hot.setDataAtCell(0, c, false);
      }
      hot.setDataAtCell(0, 3, true); // Only check x1 (Col 3)
    }
  });
  
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  console.log('Clicking Run Model...');
  await page.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });
  
  await new Promise(resolve => setTimeout(resolve, 3000));
  
  let latentErrorVisible = await page.evaluate(() => {
    const el = document.getElementById('latent_error_box');
    return el && el.style.display !== 'none' ? el.innerText : 'No latent errors';
  });
  console.log('Result for Test Case 1 (Single Indicator - Validation State):');
  console.log(latentErrorVisible);
  await page.screenshot({ path: 'test_browser/screenshot_latent_test1.png', fullPage: true });

  // --- Test Case 2: Latent Variable Name Conflicting with Observed Variable ---
  console.log('\n--- Running Test Case 2: Latent variable name same as observed variable (x1) ---');
  
  await page.evaluate(() => {
    const container = document.getElementById('input_table');
    let hot = null;
    if (window.HTMLWidgets) {
      const inst = window.HTMLWidgets.getInstance(container);
      if (inst) hot = inst.hot;
    }
    if (!hot && window.jQuery) {
      hot = window.jQuery(container).data('hot');
    }
    if (hot) {
      // Set latent name to 'x1' which is an observed variable in the dataset
      hot.setDataAtCell(0, 0, 'x1');
      // Check x1, x2, x3
      hot.setDataAtCell(0, 3, true); // x1
      hot.setDataAtCell(0, 4, true); // x2
      hot.setDataAtCell(0, 5, true); // x3
    }
  });
  
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  const generatedSyntax = await page.evaluate(() => {
    const el = document.getElementById('lavaan_model');
    return el ? el.innerText : '';
  });
  console.log('Generated syntax for Test Case 2:\n', generatedSyntax);
  
  console.log('Clicking Run Model...');
  await page.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });
  
  await new Promise(resolve => setTimeout(resolve, 3000));
  
  const validationResult = await page.evaluate(() => {
    const el = document.getElementById('latent_error_box');
    const btn = document.getElementById('run_model');
    return {
      errorVisible: el && el.style.display !== 'none',
      errorMsg: el ? el.innerText : '',
      btnDisabled: btn ? btn.disabled : false
    };
  });
  console.log('Result for Test Case 2 (Conflicting Name - Validation State):');
  console.log('- Error Visible:', validationResult.errorVisible);
  console.log('- Error Message:', validationResult.errorMsg);
  console.log('- Button Disabled:', validationResult.btnDisabled);
  await page.screenshot({ path: 'test_browser/screenshot_latent_test2.png', fullPage: true });

  await browser.close();
  console.log('\nTest completed.');
})();
