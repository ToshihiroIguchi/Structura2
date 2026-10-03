// Smoke test for plain `shiny::runApp(".", port = 8765)`: session starts, Load Data modal appears,
// and the (non-embedded) inner preload overlay is dismissed.
const puppeteer = require('puppeteer-core');
(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true, args: ['--no-sandbox']
  });
  const page = await browser.newPage();
  page.on('pageerror', e => console.log('PAGEERROR', e.message));
  await page.goto('http://localhost:8765', { waitUntil: 'domcontentloaded' });
  await page.waitForSelector('#sample_ds', { timeout: 60000 });
  await new Promise(r => setTimeout(r, 1500));
  console.log(JSON.stringify(await page.evaluate(() => {
    const c = document.getElementById('structura-preload-container');
    return {
      embedded: document.documentElement.classList.contains('structura-embedded'),
      preloadDisplay: c ? getComputedStyle(c).display : null,
      barClass: (document.getElementById('structura-preload-bar') || {}).className,
      mainVisible: getComputedStyle(document.getElementById('structura-main-app')).display
    };
  })));
  await browser.close();
})();
