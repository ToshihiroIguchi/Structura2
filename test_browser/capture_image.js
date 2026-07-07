const puppeteer = require('puppeteer-core');
const http = require('http');
const fs = require('fs');
const path = require('path');

// 1. Create a simple static server with COOP/COEP headers for ShinyLive
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

server.listen(8000, async () => {
  console.log('Server started on http://localhost:8000');
  
  try {
    await capture();
  } catch (err) {
    console.error('Error during capture:', err);
  } finally {
    server.close();
    console.log('Server stopped.');
  }
});

async function capture() {
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

  console.log('Navigating to http://localhost:8000 ...');
  await page.goto('http://localhost:8000', { waitUntil: 'networkidle2', timeout: 90000 });

  console.log('Waiting for shinylive iframe...');
  await page.waitForSelector('iframe', { timeout: 30000 });
  const appFrame = await page.$('iframe');
  const frame = await appFrame.contentFrame();
  if (!frame) throw new Error('Could not get iframe contentFrame');

  console.log('Waiting for startup modal inside iframe...');
  await frame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });
  
  console.log('Selecting HolzingerSwineford1939 demo dataset...');
  await frame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  console.log('Waiting for datatable to load...');
  await frame.waitForSelector('#datatable', { timeout: 30000 });
  
  console.log('Taking SS1: Data Loaded...');
  await new Promise(resolve => setTimeout(resolve, 1000));
  await appFrame.screenshot({ path: path.join(__dirname, '01_data_loaded.png') });
  
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
  
  // Wait a few seconds to ensure handsontable is initialized
  await new Promise(resolve => setTimeout(resolve, 3000));

  console.log('Setting up SEM model structure...');
  const setupSuccess = await frame.evaluate(async () => {
    try {
      // 1. Measurement Model: LatentVariable1 =~ x1 + x2 + x3
      const measTable = document.querySelector('#input_table');
      if (!measTable) throw new Error('Measurement table (#input_table) not found');
      
      const measHeaders = Array.from(measTable.querySelectorAll('.ht_clone_top thead th')).map(th => th.innerText.trim());
      const firstRow = measTable.querySelector('.ht_master tbody tr');
      if (!firstRow) throw new Error('First row in measurement model not found');
      const measTds = firstRow.querySelectorAll('td');
      
      console.log('measHeaders raw:', JSON.stringify(measHeaders));
      console.log('measTds count:', measTds.length);
      
      let dataMeasHeaders = measHeaders;
      if (measHeaders.length > measTds.length) {
        dataMeasHeaders = measHeaders.slice(measHeaders.length - measTds.length);
        console.log('measHeaders sliced to match tds:', JSON.stringify(dataMeasHeaders));
      }
      
      const x1_idx = dataMeasHeaders.indexOf('x1');
      const x2_idx = dataMeasHeaders.indexOf('x2');
      const x3_idx = dataMeasHeaders.indexOf('x3');
      
      if (x1_idx === -1 || x2_idx === -1 || x3_idx === -1) {
        throw new Error('x1, x2, x3 not found in measurement model headers: ' + JSON.stringify(dataMeasHeaders));
      }
      
      // Click checkboxes
      measTds[x1_idx].querySelector('input[type="checkbox"]').click();
      measTds[x2_idx].querySelector('input[type="checkbox"]').click();
      measTds[x3_idx].querySelector('input[type="checkbox"]').click();
      
      // Blur to ensure Handsontable registers and sends changes to Shiny
      document.body.click();
      return true;
    } catch (e) {
      console.error('Error in setupModel evaluation:', e.message);
      return false;
    }
  });

  if (!setupSuccess) {
    throw new Error('Failed to set up measurement model checkbox structure.');
  }

  // 2. Wait for Structural Model to update and include "LatentVariable1"
  console.log('Waiting for LatentVariable1 to appear in structural model headers...');
  const latentFound = await frame.evaluate(async () => {
    const structTable = document.querySelector('#checkbox_matrix');
    if (!structTable) return false;
    for (let i = 0; i < 20; i++) {
      const structHeaders = Array.from(structTable.querySelectorAll('.ht_clone_top thead th')).map(th => th.innerText.trim());
      if (structHeaders.includes('LatentVariable1')) {
        return true;
      }
      await new Promise(resolve => setTimeout(resolve, 500));
    }
    return false;
  });

  if (!latentFound) {
    throw new Error('LatentVariable1 did not appear in structural model headers.');
  }
  
  console.log('Taking SS2: Model Initial...');
  await new Promise(resolve => setTimeout(resolve, 1000));
  await appFrame.screenshot({ path: path.join(__dirname, '02_model_initial.png') });

  console.log('Configuring structural paths...');
  const structSuccess = await frame.evaluate(async () => {
    try {
      const structTable = document.querySelector('#checkbox_matrix');
      const structHeaders = Array.from(structTable.querySelectorAll('.ht_clone_top thead th')).map(th => th.innerText.trim());
      const structRows = Array.from(structTable.querySelectorAll('.ht_master tbody tr'));
      
      const x4Row = structRows.find(row => {
        const firstCell = row.querySelector('td');
        return firstCell && firstCell.innerText.trim() === 'x4';
      });
      
      if (!x4Row) throw new Error('Row x4 not found in structural model');
      const structTds = x4Row.querySelectorAll('td');
      
      let dataStructHeaders = structHeaders;
      if (structHeaders.length > structTds.length) {
        dataStructHeaders = structHeaders.slice(structHeaders.length - structTds.length);
      }
      
      const x7_idx = dataStructHeaders.indexOf('x7');
      const x8_idx = dataStructHeaders.indexOf('x8');
      const x9_idx = dataStructHeaders.indexOf('x9');
      const latent_idx = dataStructHeaders.indexOf('LatentVariable1');
      
      if (x7_idx === -1 || x8_idx === -1 || x9_idx === -1 || latent_idx === -1) {
        throw new Error('Target columns not found in structural model headers');
      }
      
      structTds[x7_idx].querySelector('input[type="checkbox"]').click();
      structTds[x8_idx].querySelector('input[type="checkbox"]').click();
      structTds[x9_idx].querySelector('input[type="checkbox"]').click();
      structTds[latent_idx].querySelector('input[type="checkbox"]').click();
      
      return true;
    } catch (e) {
      console.error('Error configuring structural paths:', e.message);
      return false;
    }
  });

  if (!structSuccess) {
    throw new Error('Failed to configure structural paths.');
  }

  console.log('Taking SS3: Structural Interactions...');
  await new Promise(resolve => setTimeout(resolve, 1000));
  await appFrame.screenshot({ path: path.join(__dirname, '03_structural_interactions.png') });

  console.log('Model structure configured. Clicking Run / Update Model...');
  await frame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for path diagram rendering...');
  await frame.waitForSelector('#sem_plot_container svg', { timeout: 30000 });
  
  // Extra wait for layout and rendering stabilization
  await new Promise(resolve => setTimeout(resolve, 2000));

  console.log('Taking SS4: Model Execution...');
  await appFrame.screenshot({ path: path.join(__dirname, '04_model_execution.png') });
  
  // Also save to default image.png at root for backward compatibility
  await appFrame.screenshot({ path: path.join(__dirname, '..', 'image.png') });
  
  console.log('All screenshots captured successfully!');
  await browser.close();
}
