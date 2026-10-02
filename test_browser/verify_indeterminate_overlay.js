const puppeteer = require('puppeteer-core');
const fs = require('fs');

(async () => {
  console.log('Starting Browser Verification for Indeterminate Progress Overlay...');

  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 800 });
  await page.setCacheEnabled(false);

  page.on('console', msg => {
    console.log(`[BROWSER CONSOLE] ${msg.text()}`);
  });

  page.on('pageerror', err => {
    console.error(`[BROWSER ERROR] ${err.message}`);
  });

  const startTime = Date.now();
  console.log('Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });

  // Take screenshot at ~2s to capture active Indeterminate Overlay
  await new Promise(r => setTimeout(r, 2000));
  await page.screenshot({ path: 'test_browser/overlay_active.png' });
  console.log(`[${((Date.now() - startTime) / 1000).toFixed(2)}s] Captured active overlay screenshot: test_browser/overlay_active.png`);

  // Verify that the overlay is present in DOM
  const overlayExists = await page.evaluate(() => !!document.getElementById('structura-splash-overlay'));
  console.log(`Overlay present in DOM at 2s: ${overlayExists}`);

  // Wait for overlay to fade out and be removed from DOM
  console.log('Waiting for app ready signal and overlay removal...');
  await page.waitForFunction(() => !document.getElementById('structura-splash-overlay'), { timeout: 45000 });
  const totalMs = Date.now() - startTime;
  console.log(`[${(totalMs / 1000).toFixed(2)}s] Overlay successfully removed! Structura2 is fully ready.`);

  // Wait 1s for UI to settle, then take screenshot of loaded app
  await new Promise(r => setTimeout(r, 1000));
  await page.screenshot({ path: 'test_browser/overlay_completed.png' });
  console.log('Captured completed app screenshot: test_browser/overlay_completed.png');

  await browser.close();

  console.log('\n==================================================');
  console.log('   VERIFICATION SUCCESSFUL: Overlay & Ready Signal ');
  console.log(`   Total startup time: ${(totalMs / 1000).toFixed(2)}s`);
  console.log('==================================================');
})();
