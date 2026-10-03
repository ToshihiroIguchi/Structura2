// Logs WebR worker message types and Structura2 stage broadcasts with timestamps (cold start).
const puppeteer = require('puppeteer-core');
(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true, args: ['--no-sandbox']
  });
  const page = await browser.newPage();
  await page.setCacheEnabled(false);
  await page.evaluateOnNewDocument(() => {
    const t0 = performance.now();
    const log = (m) => console.log('EVT ' + Math.round(performance.now() - t0) + ' ' + m);
    const seen = {};
    const Orig = window.Worker;
    window.Worker = function (url, opts) {
      log('worker_created ' + url);
      const w = new Orig(url, opts);
      w.addEventListener('message', (e) => {
        const d = e.data || {};
        const k = (d.type || '?') + ':' + ((d.data && d.data.type) || (d.obj && d.obj.type) || '');
        seen[k] = (seen[k] || 0) + 1;
        if (seen[k] <= 2) log('worker_msg ' + k);
      });
      return w;
    };
    new BroadcastChannel('structura-progress').onmessage = (e) => log('stage ' + JSON.stringify(e.data));
    window.addEventListener('message', (e) => { if (e.data && e.data.type) log('postMessage ' + e.data.type); });
  });
  page.on('console', m => { const t = m.text(); if (t.startsWith('EVT ')) console.log(t); });
  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 25000));
  await browser.close();
})();
