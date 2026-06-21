const puppeteer = require('puppeteer-core');
const path = require('path');
const fs = require('fs');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  const runTestForFile = async (fileName, outputName) => {
    console.log(`\n--- Testing file: ${fileName} ---`);
    console.log('Navigating to http://localhost:8000 ...');
    await page.goto('http://localhost:8000', { waitUntil: 'networkidle2', timeout: 90000 });
    
    console.log('Waiting for shinylive iframe to load...');
    await page.waitForSelector('iframe', { timeout: 30000 });
    const iframeElement = await page.$('iframe');
    const frame = await iframeElement.contentFrame();
    if (!frame) throw new Error('Could not get contentFrame from iframe');

    console.log('Waiting for file input...');
    const fileInput = await frame.waitForSelector('input[type="file"]', { timeout: 90000 });
    
    const filePath = path.resolve(__dirname, '..', fileName);
    console.log(`Uploading file: ${filePath}`);
    await fileInput.uploadFile(filePath);

    console.log('Waiting for modal to dismiss and data to load...');
    try {
      await frame.waitForSelector('#datatable', { timeout: 30000 });
      console.log('Data loaded successfully.');
    } catch (err) {
      console.error('Data table failed to load. Checking for error modal...');
      const modalText = await frame.evaluate(() => {
        const modal = document.querySelector('.modal-body');
        return modal ? modal.innerText : null;
      });
      if (modalText) {
        console.error('Error modal text:', modalText);
      }
      await page.screenshot({ path: `C:\\Users\\toshi\\python\\Structura2\\test_browser\\error_${outputName}.png` });
      return;
    }

    // Wait 2 seconds for DT rendering
    await new Promise(resolve => setTimeout(resolve, 2000));

    // Inspect columns of the datatable
    const headers = await frame.evaluate(() => {
      const ths = document.querySelectorAll('#datatable th');
      return Array.from(ths).map(th => th.innerText.trim());
    });
    console.log('Detected columns in datatable:', headers);

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
    console.log('Model tab tables selector found. Waiting 3s for cells to render...');
    await new Promise(resolve => setTimeout(resolve, 3000));

    const varNames = headers.filter(h => h !== '');
    console.log('Variables available for model:', varNames);

    if (varNames.length >= 2) {
      const eq = `${varNames[0]} ~ ${varNames[1]}`;
      console.log(`Entering manual equation: ${eq}`);
      await frame.evaluate((eqText) => {
        const textarea = document.querySelector('#extra_eq');
        if (textarea) {
          textarea.value = eqText;
          textarea.dispatchEvent(new Event('input', { bubbles: true }));
          textarea.dispatchEvent(new Event('change', { bubbles: true }));
        }
      }, eq);

      await new Promise(resolve => setTimeout(resolve, 1500));

      console.log('Clicking Run Model button...');
      await frame.evaluate(() => {
        const btn = document.querySelector('#run_model');
        if (btn) btn.click();
      });

      console.log('Waiting 5s for model execution and plot rendering...');
      await new Promise(resolve => setTimeout(resolve, 5000));

      // Try checking if we have visual elements or logs
      const plotExists = await frame.evaluate(() => {
        const plot = document.querySelector('#sem_plot_ui');
        return plot ? plot.innerHTML.includes('svg') || plot.innerHTML.includes('Error') : false;
      });
      console.log('Path diagram rendered element exists:', plotExists);

      const syntax = await frame.evaluate(() => {
        const el = document.querySelector('#lavaan_model');
        return el ? el.innerText : '';
      });
      console.log('Generated lavaan syntax:\n', syntax);

      const diagnostics = await frame.evaluate(() => {
        const el = document.getElementById('fit_alert_box');
        return el && el.style.display !== 'none' ? el.innerText : 'No alerts';
      });
      console.log('Diagnostics:', diagnostics);
    }

    console.log('Taking screenshot...');
    const screenshotPath = `C:\\Users\\toshi\\python\\Structura2\\test_browser\\screenshot_${outputName}.png`;
    await page.screenshot({ path: screenshotPath, fullPage: true });
    console.log(`Saved screenshot to ${screenshotPath}`);
  };

  try {
    await runTestForFile('test_utf8.csv', 'utf8');
    await runTestForFile('test_sjis.csv', 'sjis');
  } catch (err) {
    console.error('Test script crashed:', err);
  } finally {
    await browser.close();
    console.log('Browser closed. Test execution finished.');
  }
})();
