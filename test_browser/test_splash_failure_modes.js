// Verifies the host-page overlay failure modes:
//  1. a 'structura-error' message shows the error box + Reload and never auto-dismisses,
//  2. a slow start (heavily throttled network) shows the "taking longer" note after 45 s
//     instead of hiding the overlay, and [Keep waiting] hides only the note.
const puppeteer = require('puppeteer-core');
const path = require('path');
const exe = 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe';
const url = 'http://localhost:8100';

async function state(page) {
  return page.evaluate(() => ({
    overlay: !!document.getElementById('structura-splash-overlay'),
    slowHidden: (document.getElementById('structura-splash-slow') || {}).hidden,
    errorHidden: (document.getElementById('structura-splash-error') || {}).hidden,
    errorText: (document.getElementById('structura-error-text') || {}).textContent,
    status: (document.getElementById('structura-splash-status') || {}).textContent,
    meta: (document.getElementById('structura-splash-meta') || {}).textContent
  }));
}

(async () => {
  const browser = await puppeteer.launch({ executablePath: exe, headless: true, defaultViewport: { width: 1200, height: 800 }, args: ['--no-sandbox'] });

  // 1. Error path
  let page = await browser.newPage();
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 1500));
  await page.evaluate(() => window.postMessage({ type: 'structura-error', message: 'simulated failure' }, '*'));
  await new Promise(r => setTimeout(r, 600));
  console.log('error state:', JSON.stringify(await state(page)));
  await page.screenshot({ path: path.join(__dirname, 'splash_error.png') });
  await new Promise(r => setTimeout(r, 15000));
  console.log('error state after 15 s (overlay must remain):', JSON.stringify(await state(page)));
  await page.close();

  // 2. Slow path
  page = await browser.newPage();
  await page.setCacheEnabled(false);
  const cdp = await page.createCDPSession();
  await cdp.send('Network.enable');
  await cdp.send('Network.emulateNetworkConditions', { offline: false, latency: 150, downloadThroughput: (2 * 1024 * 1024) / 8, uploadThroughput: (1 * 1024 * 1024) / 8 });
  await page.goto(url, { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 50000));
  console.log('slow state at ~50 s:', JSON.stringify(await state(page)));
  await page.screenshot({ path: path.join(__dirname, 'splash_slow.png') });
  await page.evaluate(() => document.getElementById('structura-slow-wait').click());
  await new Promise(r => setTimeout(r, 1500));
  console.log('after Keep waiting:', JSON.stringify(await state(page)));
  await browser.close();
})();
