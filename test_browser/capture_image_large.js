const puppeteer = require('puppeteer-core');
const http = require('http');
const fs = require('fs');
const path = require('path');

const MIME_TYPES = {
  '.html': 'text/html',
  '.css': 'text/css',
  '.js': 'text/javascript',
  '.json': 'application/json',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.gif': 'image/gif',
  '.svg': 'image/svg+xml',
  '.ico': 'image/x-icon',
  '.wasm': 'application/wasm'
};

const server = http.createServer((req, res) => {
  let urlPath = req.url.split('?')[0].split('#')[0];
  let filePath = path.join(__dirname, '..', 'site', urlPath === '/' ? 'index.html' : urlPath);

  const ext = path.extname(filePath);
  const contentType = MIME_TYPES[ext] || 'application/octet-stream';

  fs.readFile(filePath, (err, content) => {
    if (err) {
      res.writeHead(404, { 'Content-Type': 'text/html' });
      res.end('<h1>404 Not Found</h1>', 'utf-8');
    } else {
      const headers = {
        'Content-Type': contentType,
        'Cross-Origin-Opener-Policy': 'same-origin',
        'Cross-Origin-Embedder-Policy': 'require-corp'
      };
      if (filePath.endsWith('shinylive-sw.js')) {
        headers['Service-Worker-Allowed'] = '/';
      }
      res.writeHead(200, headers);
      res.end(content, 'utf-8');
    }
  });
});

server.listen(8100, async () => {
  console.log('Server started on http://localhost:8100');
  
  try {
    await captureLarge();
  } catch (err) {
    console.error('Error during capture:', err);
  } finally {
    server.close();
    console.log('Server stopped.');
  }
});

async function captureLarge() {
  console.log('Launching browser...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    defaultViewport: {
      width: 1600,
      height: 1000
    },
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  try {
    console.log('Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 90000 });

  console.log('Waiting for shinylive iframe...');
  await page.waitForSelector('iframe', { timeout: 30000 });
  const appFrame = await page.$('iframe');
  const frame = await appFrame.contentFrame();
  if (!frame) throw new Error('Could not get iframe contentFrame');

  console.log('Waiting for file upload dialog...');
  await frame.waitForSelector('input[type="file"]', { timeout: 90000 });
  const fileInput = await frame.$('input[type="file"]');
  
  console.log('Uploading test_large.csv (50 variables)...');
  await fileInput.uploadFile(path.join(__dirname, '..', 'test_large.csv'));

  console.log('Waiting for modal to disappear...');
  await frame.waitForSelector('.modal-dialog', { hidden: true, timeout: 30000 });

  console.log('Waiting for datatable to load large data columns...');
  await frame.waitForFunction(() => {
    const ths = Array.from(document.querySelectorAll('#datatable th')).map(th => th.innerText.trim());
    return ths.includes('v1');
  }, { timeout: 45000 });
  console.log('Large data loaded into datatable successfully!');
  
  console.log('Taking SS1: Large Data Loaded...');
  await new Promise(resolve => setTimeout(resolve, 1500));
  await appFrame.screenshot({ path: path.join(__dirname, '05_large_data_loaded.png') });
  
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

  console.log('Waiting for Model tables to render...');
  await frame.waitForSelector('#input_table', { timeout: 15000 });
  await frame.waitForSelector('#checkbox_matrix', { timeout: 15000 });
  
  console.log('Waiting 8s for tables and reactive values to settle...');
  await new Promise(resolve => setTimeout(resolve, 8000));

  console.log('Setting up Measurement Model (v1, v2, v3 for LatentVariable1)...');
  const setupSuccess = await frame.evaluate(async () => {
    try {
      const hotWidget = window.HTMLWidgets.find('#input_table');
      if (!hotWidget) throw new Error('HTMLWidget #input_table not found');
      const hot = hotWidget.hot;
      if (!hot) throw new Error('Handsontable instance not found on #input_table');
      
      const colNames = hot.getColHeader();
      const v1_idx = colNames.indexOf('v1');
      const v2_idx = colNames.indexOf('v2');
      const v3_idx = colNames.indexOf('v3');
      
      if (v1_idx === -1 || v2_idx === -1 || v3_idx === -1) {
        throw new Error('v1, v2, or v3 column not found in Handsontable: ' + JSON.stringify(colNames));
      }
      
      hot.setDataAtCell(0, v1_idx, true);
      hot.setDataAtCell(0, v2_idx, true);
      hot.setDataAtCell(0, v3_idx, true);
      
      if (window.Shiny) {
        window.Shiny.onInputChange('input_table', {
          data: hot.getData(),
          changes: { event: "afterChange", changes: [[0, v1_idx, false, true]], source: "edit" },
          params: hot.params
        });
      }
      
      return true;
    } catch (e) {
      console.error('Error in setupModel evaluation:', e.message);
      return false;
    }
  });

  if (!setupSuccess) {
    throw new Error('Failed to set up measurement model checkbox structure.');
  }

  console.log('Waiting for LatentVariable1 to appear in structural model headers...');
  const latentFound = await frame.evaluate(async () => {
    const hotWidget = window.HTMLWidgets.find('#checkbox_matrix');
    if (!hotWidget) return false;
    const hot = hotWidget.hot;
    if (!hot) return false;
    for (let i = 0; i < 20; i++) {
      const colHeaders = hot.getColHeader();
      if (colHeaders && colHeaders.includes('LatentVariable1')) {
        return true;
      }
      await new Promise(resolve => setTimeout(resolve, 500));
    }
    return false;
  });

  if (!latentFound) {
    throw new Error('LatentVariable1 did not appear in structural model headers.');
  }
  
  console.log('Taking SS2: Large Model Initial (51x51 table with diagonal cells grayed-out)...');
  await new Promise(resolve => setTimeout(resolve, 1500));
  await appFrame.screenshot({ path: path.join(__dirname, '06_large_model_initial.png') });

  console.log('Configuring structural paths...');
  const structSuccess = await frame.evaluate(async () => {
    try {
      const hotWidget = window.HTMLWidgets.find('#checkbox_matrix');
      if (!hotWidget) throw new Error('HTMLWidget #checkbox_matrix not found');
      const hot = hotWidget.hot;
      if (!hot) throw new Error('Handsontable instance not found on #checkbox_matrix');
      
      const colNames = hot.getColHeader();
      const rowData = hot.getData();
      
      const v4_row_idx = rowData.findIndex(r => r[0] === 'v4');
      const v10_row_idx = rowData.findIndex(r => r[0] === 'v10');
      
      if (v4_row_idx === -1 || v10_row_idx === -1) {
        throw new Error('v4 or v10 row not found in structural model data');
      }
      
      const v10_col_idx = colNames.indexOf('v10');
      const v20_col_idx = colNames.indexOf('v20');
      const latent_col_idx = colNames.indexOf('LatentVariable1');
      
      if (v10_col_idx === -1 || v20_col_idx === -1 || latent_col_idx === -1) {
        throw new Error('v10, v20 or LatentVariable1 not found in structural model columns');
      }
      
      hot.setDataAtCell(v4_row_idx, v10_col_idx, true);
      hot.setDataAtCell(v10_row_idx, v20_col_idx, true);
      hot.setDataAtCell(v10_row_idx, latent_col_idx, true);
      
      if (window.Shiny) {
        window.Shiny.onInputChange('checkbox_matrix', {
          data: hot.getData(),
          changes: { event: "afterChange", changes: [[v4_row_idx, v10_col_idx, false, true]], source: "edit" },
          params: hot.params
        });
      }
      
      return true;
    } catch (e) {
      console.error('Error configuring structural paths:', e.message);
      return false;
    }
  });

  if (!structSuccess) {
    throw new Error('Failed to configure structural paths.');
  }

  console.log('Taking SS3: Large Structural Interactions (colored cells and checked paths)...');
  await new Promise(resolve => setTimeout(resolve, 1500));
  await appFrame.screenshot({ path: path.join(__dirname, '07_large_structural_interactions.png') });

  console.log('Clicking Run / Update Model...');
  await frame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting 5s for model estimation to complete...');
  await new Promise(resolve => setTimeout(resolve, 5000));
  
  console.log('Taking SS4: Large Model Execution...');
  await appFrame.screenshot({ path: path.join(__dirname, '08_large_model_execution.png') });
  
    console.log('All screenshots captured successfully!');
  } catch (err) {
    console.log('Test failed. Taking error screenshot...');
    await page.screenshot({ path: path.join(__dirname, 'error_large_run.png') });
    throw err;
  }
  await browser.close();
}
