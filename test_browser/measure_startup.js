const puppeteer = require('puppeteer-core');

(async () => {
  const startTime = Date.now();
  console.log(`[0.000s] Starting Puppeteer...`);

  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  
  // Disable cache to simulate a cold start
  await page.setCacheEnabled(false);

  const logs = [];
  const addLog = (msg) => {
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(3);
    const logStr = `[${elapsed}s] ${msg}`;
    console.log(logStr);
    logs.push(logStr);
  };

  page.on('console', msg => {
    addLog(`BROWSER LOG: ${msg.text()}`);
  });

  page.on('pageerror', err => {
    addLog(`BROWSER ERROR: ${err.message}`);
  });

  // Track network requests passively without interception
  const requests = {};
  page.on('request', req => {
    const url = req.url();
    requests[url] = { start: Date.now() };
  });

  page.on('requestfinished', req => {
    const url = req.url();
    if (requests[url]) {
      const elapsed = Date.now() - requests[url].start;
      const resp = req.response();
      const size = resp ? (resp.headers()['content-length'] || 'unknown') : 0;
      
      const isWebR = url.includes('webr') || url.includes('shinylive') || url.includes('.wasm') || url.includes('app.json');
      const isSlow = elapsed > 200;
      if (isWebR || isSlow) {
        addLog(`Network finished: ${url.substring(0, 100)}${url.length > 100 ? '...' : ''} (${elapsed}ms, size: ${size} bytes)`);
      }
    }
  });

  page.on('requestfailed', req => {
    const url = req.url();
    addLog(`Network FAILED: ${url}`);
  });

  addLog('Navigating to http://localhost:8000 ...');
  try {
    await page.goto('http://localhost:8000', { waitUntil: 'load', timeout: 120000 });
    addLog('Main page loaded.');
  } catch (err) {
    addLog(`Navigation error: ${err.message}`);
  }

  addLog('Waiting for shinylive iframe...');
  let frame;
  try {
    await page.waitForSelector('iframe', { timeout: 30000 });
    const iframeElement = await page.$('iframe');
    frame = await iframeElement.contentFrame();
    addLog('shinylive iframe obtained.');
  } catch (err) {
    addLog(`iframe error: ${err.message}`);
  }

  if (frame) {
    addLog('Waiting for #sample_ds modal inside iframe...');
    try {
      await frame.waitForSelector('#sample_ds', { timeout: 120000 });
      addLog('=== SUCCESS: #sample_ds modal found. Application is fully loaded. ===');
    } catch (err) {
      addLog(`Modal wait error: ${err.message}`);
    }
  }

  await browser.close();
  addLog('Finished profiling.');
})();
