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
    let reqCount = 0;
    const OrigWorker = window.Worker;
    window.Worker = function(scriptURL, options) {
      const worker = new OrigWorker(scriptURL, options);
      worker.addEventListener('message', (e) => {
        const d = e.data;
        if (d && (d.type === 'request' || d.type === 'response')) {
          reqCount++;
          console.log(`WORKER_PROGRESS_PING: count=${reqCount}`);
        }
      });
      return worker;
    };
  });

  let pings = 0;
  page.on('console', msg => {
    const t = msg.text();
    if (t.startsWith('WORKER_PROGRESS_PING:')) {
      pings++;
      if (pings % 5 === 0) console.log(`Received ${pings} worker pings`);
    }
  });

  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 6000));
  await browser.close();
  console.log(`Total worker communication events intercepted: ${pings}`);
})();
