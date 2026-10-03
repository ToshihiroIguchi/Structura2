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
    const OrigWorker = window.Worker;
    window.Worker = function(scriptURL, options) {
      console.log('HOOKED_WORKER_CREATED:', scriptURL);
      const worker = new OrigWorker(scriptURL, options);
      worker.addEventListener('message', (e) => {
        const d = e.data;
        if (d && d.type) {
          console.log('WORKER_MSG_TYPE:', d.type, JSON.stringify(d.data || '').substring(0, 100));
        }
      });
      return worker;
    };
  });

  page.on('console', msg => {
    const t = msg.text();
    if (t.startsWith('HOOKED_') || t.startsWith('WORKER_MSG_')) {
      console.log(t);
    }
  });

  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 6000));
  await browser.close();
})();
