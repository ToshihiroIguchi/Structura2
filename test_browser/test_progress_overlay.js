const puppeteer = require('puppeteer-core');
const fs = require('fs');

(async () => {
  console.log('Testing Real Progress Overlay in Structura2 Shinylive...');

  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 800 });
  await page.setCacheEnabled(false);

  const progressUpdates = [];
  const startTime = Date.now();

  // Monitor DOM changes on the progress elements
  await page.exposeFunction('onProgressUpdate', (pct, status) => {
    const elapsed = ((Date.now() - startTime) / 1000).toFixed(2);
    const entry = `[${elapsed}s] ${pct} - ${status}`;
    console.log(entry);
    progressUpdates.push(entry);
  });

  await page.evaluateOnNewDocument(() => {
    window.addEventListener('DOMContentLoaded', () => {
      const pctElem = document.getElementById('structura-progress-pct');
      const statusElem = document.getElementById('structura-progress-status');
      if (pctElem && statusElem) {
        const observer = new MutationObserver(() => {
          window.onProgressUpdate(pctElem.innerText, statusElem.innerText);
        });
        observer.observe(pctElem, { childList: true, characterData: true, subtree: true });
        observer.observe(statusElem, { childList: true, characterData: true, subtree: true });
      }
    });
  });

  console.log('Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });

  // Take screenshot at ~2s (Early stage: WebR download)
  await new Promise(r => setTimeout(r, 2000));
  await page.screenshot({ path: 'test_browser/progress_stage1.png' });
  console.log('Saved screenshot: test_browser/progress_stage1.png');

  // Take screenshot at ~5s (Mid stage: Packages loading)
  await new Promise(r => setTimeout(r, 3000));
  await page.screenshot({ path: 'test_browser/progress_stage2.png' });
  console.log('Saved screenshot: test_browser/progress_stage2.png');

  // Wait for overlay to disappear
  console.log('Waiting for overlay to be removed (App Ready)...');
  await page.waitForFunction(() => !document.getElementById('structura-splash-overlay'), { timeout: 45000 });
  console.log('Overlay successfully removed!');

  // Take final screenshot
  await page.screenshot({ path: 'test_browser/progress_final.png' });
  console.log('Saved screenshot: test_browser/progress_final.png');

  await browser.close();

  console.log('\n=== PROGRESS UPDATES RECORDED ===');
  progressUpdates.forEach(p => console.log(p));
  console.log(`\nTotal distinct progress updates recorded: ${progressUpdates.length}`);

  if (progressUpdates.length > 5) {
    console.log('\nSUCCESS: Real progress bar updates dynamically based on WebR events!');
  } else {
    console.error('FAIL: Progress updates were too few.');
    process.exit(1);
  }
})();
