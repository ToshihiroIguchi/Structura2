const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Debugging Shinylive iframe and frames on http://localhost:8080 ...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  await page.goto('http://localhost:8080', { waitUntil: 'networkidle2', timeout: 60000 });

  console.log('Waiting 30s for Shinylive iframe to mount...');
  await new Promise(r => setTimeout(r, 30000));

  const frames = page.frames();
  console.log('Frame count:', frames.length);
  for (let i = 0; i < frames.length; i++) {
    console.log(`Frame ${i} URL:`, frames[i].url());
  }

  // Find the target frame (either main page or iframe)
  let targetFrame = page.mainFrame();
  for (const frame of frames) {
    if (frame !== page.mainFrame()) {
      targetFrame = frame;
      break;
    }
  }

  console.log('Waiting for HolzingerSwineford1939 in target frame...');
  try {
    await targetFrame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 40000 });
    console.log('FOUND HolzingerSwineford1939 inside frame!');
  } catch (e) {
    console.error('Failed to find input in frame:', e.message);
  }

  await page.screenshot({ path: 'test_browser/debug_iframe_screenshot.png', fullPage: true });
  await browser.close();
})();
