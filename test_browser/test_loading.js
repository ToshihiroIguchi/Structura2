const puppeteer = require('puppeteer-core');
const fs = require('fs');
const path = require('path');

(async () => {
  console.log('Launching browser to test Structura2 loading UI...');
  const browser = await puppeteer.launch({
    executablePath: fs.existsSync('C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe') 
      ? 'C:\\Program Files\\Google\\Chrome\\Application\\chrome.exe'
      : 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  
  // Monitor console logs and page errors
  const consoleLogs = [];
  page.on('console', msg => {
    const text = msg.text();
    consoleLogs.push(text);
    console.log('BROWSER LOG:', text);
  });
  page.on('pageerror', err => {
    console.error('BROWSER ERROR:', err.message);
  });

  console.log('Navigating to http://localhost:8000 ...');
  try {
    await page.goto('http://localhost:8000', { waitUntil: 'domcontentloaded' });
    await new Promise(resolve => setTimeout(resolve, 800)); // Delay for page/sw redirect stability
  } catch (err) {
    console.error('Navigation failed.', err);
    await browser.close();
    process.exit(1);
  }

  // 1. Verify early stage 1 HTML placeholder and CSS styling
  console.log('Checking initial splash screen (Stage 1)...');
  
  // Read site/index.html directly from disk to verify the static build output
  const indexHtmlPath = path.join(__dirname, '../site/index.html');
  const indexHtmlContent = fs.readFileSync(indexHtmlPath, 'utf8');
  const diskContainsLoader = indexHtmlContent.includes('Loading WebR Engine...') && indexHtmlContent.includes('structura-spin');
  console.log('Verified index.html on disk contains Stage 1 Loader HTML:', diskContainsLoader);

  let splashHTML = null;
  let bodyBgColor = null;
  try {
    splashHTML = await page.evaluate(() => {
      const root = document.getElementById('root');
      return root ? root.innerHTML : null;
    });
    bodyBgColor = await page.evaluate(() => {
      return window.getComputedStyle(document.body).backgroundColor;
    });
  } catch (e) {
    console.log(`Early evaluate failed: ${e.message}`);
  }

  console.log('Initial Splash HTML contains WebR Loader text (runtime):', splashHTML && splashHTML.includes('Loading WebR Engine...'));
  console.log('Initial Body Background Color (should be dark/slate):', bodyBgColor);

  // Capture Stage 1 loading screenshot
  await page.screenshot({ path: 'test_browser/screenshot_stage1_loading.png' });
  console.log('Screenshot of Stage 1 Loading saved to test_browser/screenshot_stage1_loading.png');

  // Wait for shinylive iframe to load
  console.log('Waiting for shinylive iframe...');
  let frame = null;
  try {
    await page.waitForSelector('iframe', { timeout: 15000 });
    const iframeElement = await page.$('iframe');
    frame = await iframeElement.contentFrame();
    if (!frame) throw new Error('Could not get contentFrame from iframe');
  } catch (err) {
    console.error('Failed to get shinylive iframe.', err);
    await browser.close();
    process.exit(1);
  }

  // 2. Wait and check the progressive advancement of the loading bar inside the iframe
  console.log('Waiting for structura-preload-bar to appear inside the iframe...');
  try {
    await frame.waitForSelector('#structura-preload-bar', { timeout: 25000 });
    console.log('structura-preload-bar is now visible in the DOM.');
  } catch (err) {
    console.error('structura-preload-bar failed to appear within 25 seconds.', err);
    await page.screenshot({ path: 'test_browser/screenshot_error.png' });
    await browser.close();
    process.exit(1);
  }
  
  let barWidthStart = null;
  let preloadStatusStart = null;
  try {
    barWidthStart = await frame.evaluate(() => {
      const bar = document.getElementById('structura-preload-bar');
      return bar ? bar.style.width : null;
    });
    preloadStatusStart = await frame.evaluate(() => {
      const status = document.getElementById('structura-preload-status');
      return status ? status.innerText : null;
    });
  } catch (e) {
    console.log(`Failed to evaluate inside iframe: ${e.message}`);
  }

  console.log('Progress bar width when initialized:', barWidthStart);
  console.log('Preload Status message when initialized:', preloadStatusStart);

  // Capture Stage 2 loading screenshot right after initialization (active loader)
  await page.screenshot({ path: 'test_browser/screenshot_stage2_progress.png' });
  console.log('Screenshot of Stage 2 Active Progress saved to test_browser/screenshot_stage2_progress.png');

  console.log('Waiting 3 seconds to let progress advance...');
  await new Promise(resolve => setTimeout(resolve, 3000));

  let barWidthLater = null;
  let preloadStatusLater = null;
  try {
    barWidthLater = await frame.evaluate(() => {
      const bar = document.getElementById('structura-preload-bar');
      return bar ? bar.style.width : null;
    });
    preloadStatusLater = await frame.evaluate(() => {
      const status = document.getElementById('structura-preload-status');
      return status ? status.innerText : null;
    });
  } catch (e) {
    console.log(`Failed to evaluate inside iframe: ${e.message}`);
  }

  console.log('Progress bar width after 3s (should be > initial):', barWidthLater);
  console.log('Preload Status message after 3s:', preloadStatusLater);

  // Capture Stage 2 complete screenshot
  await page.screenshot({ path: 'test_browser/screenshot_stage2_complete.png' });
  console.log('Screenshot of Stage 2 Complete saved to test_browser/screenshot_stage2_complete.png');

  // 3. Wait for the application to fully boot (inside the iframe)
  console.log('Waiting for application to fully load (data load modal to appear)...');
  try {
    await frame.waitForSelector('input[name="sample_ds"]', { timeout: 45000 });
    console.log('Application loaded successfully! Data upload modal is now visible.');
    
    // Capture fully loaded application screenshot
    await page.screenshot({ path: 'test_browser/screenshot_loaded.png' });
    console.log('Screenshot of Loaded Application saved to test_browser/screenshot_loaded.png');
  } catch (err) {
    console.error('Failed to load application within 45 seconds.', err);
    await page.screenshot({ path: 'test_browser/screenshot_error.png' });
    await browser.close();
    process.exit(1);
  }

  await browser.close();
  console.log('Browser test completed successfully.');
})();
