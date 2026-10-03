const puppeteer = require('puppeteer-core');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setCacheEnabled(false);

  await page.evaluateOnNewDocument(() => {
    window.__perfResources = [];
    const observer = new PerformanceObserver((list) => {
      for (const entry of list.getEntries()) {
        window.__perfResources.push({ name: entry.name, duration: entry.duration });
        console.log(`PERF_RESOURCE: ${entry.name}`);
      }
    });
    observer.observe({ entryTypes: ['resource'] });
  });

  page.on('console', msg => {
    const text = msg.text();
    if (text.startsWith('PERF_RESOURCE:')) {
      console.log(text.replace('http://localhost:8100/', ''));
    }
  });

  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 6000));
  await browser.close();
})();
