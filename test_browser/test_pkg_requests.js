// Lists the R package archives requested during a cold start, to verify that deferred packages are not fetched.
const puppeteer = require('puppeteer-core');
(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true, args: ['--no-sandbox']
  });
  const page = await browser.newPage();
  await page.setCacheEnabled(false);
  const reqs = [];
  const t0 = Date.now();
  page.on('requestfinished', r => {
    const u = r.url();
    if (/\.tgz|\/packages\/|repo\.r-wasm|metadata\.rds|deferred\.txt/.test(u)) reqs.push(((Date.now() - t0) / 1000).toFixed(1) + 's ' + u.replace('http://localhost:8100/', ''));
  });
  await page.goto('http://localhost:8100', { waitUntil: 'domcontentloaded' });
  await new Promise(r => setTimeout(r, 30000));
  console.log(reqs.join('\n'));
  console.log('package-ish requests:', reqs.length);
  await browser.close();
})();
