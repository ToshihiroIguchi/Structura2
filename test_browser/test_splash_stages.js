// Samples the host-page loading overlay during a cold start and verifies that
//  - the bar is monotonic and only reaches 100% on the real ready signal,
//  - the overlay is only removed after the Load Data modal exists,
//  - milestones are recorded in order.
// Usage: node test_browser/test_splash_stages.js [url] [--throttle]
const puppeteer = require('puppeteer-core');
const path = require('path');

const url = process.argv.find(a => a.startsWith('http')) || 'http://localhost:8100';
const throttle = process.argv.includes('--throttle');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    defaultViewport: { width: 1200, height: 800 },
    args: ['--no-sandbox']
  });
  const page = await browser.newPage();
  await page.setCacheEnabled(false);
  if (throttle) {
    const cdp = await page.createCDPSession();
    await cdp.send('Network.enable');
    await cdp.send('Network.emulateNetworkConditions', {
      offline: false, latency: 80, downloadThroughput: (20 * 1024 * 1024) / 8, uploadThroughput: (10 * 1024 * 1024) / 8
    });
  }
  page.on('pageerror', e => console.log('PAGEERROR', e.message));

  const t0 = Date.now();
  await page.goto(url, { waitUntil: 'domcontentloaded' });

  const samples = [];
  let removedAt = null;
  let modalAtRemoval = null;
  let shots = 0;
  while (Date.now() - t0 < 120000) {
    const s = await page.evaluate(() => {
      const ov = document.getElementById('structura-splash-overlay');
      const fill = document.getElementById('structura-progress-fill');
      const iframe = document.querySelector('iframe');
      let modal = false;
      try { modal = !!(iframe && iframe.contentDocument && iframe.contentDocument.querySelector('.modal-dialog')); } catch (e) {}
      return {
        overlay: !!ov,
        pct: fill ? parseFloat(fill.style.width) || 0 : null,
        status: (document.getElementById('structura-splash-status') || {}).textContent,
        meta: (document.getElementById('structura-splash-meta') || {}).textContent,
        modal,
        milestones: window.__structuraMilestones || null
      };
    }).catch(() => null);
    if (!s) { await new Promise(r => setTimeout(r, 300)); continue; }
    s.t = ((Date.now() - t0) / 1000).toFixed(1);
    samples.push(s);
    if (s.overlay && shots < 3 && samples.length % 6 === 0) {
      await page.screenshot({ path: path.join(__dirname, `splash_stage_${++shots}.png`) });
    }
    if (!s.overlay) { removedAt = s.t; modalAtRemoval = s.modal; break; }
    await new Promise(r => setTimeout(r, 500));
  }

  let last = -1, monotonic = true;
  for (const s of samples) {
    if (s.pct !== null) { if (s.pct < last) monotonic = false; last = s.pct; }
  }
  console.log('--- samples (every ~3rd) ---');
  samples.filter((_, i) => i % 3 === 0).forEach(s => console.log(s.t + 's', s.pct + '%', '|', s.status, '|', s.meta));
  const lastWithMs = [...samples].reverse().find(s => s.milestones);
  console.log('milestones (ms):', JSON.stringify(lastWithMs && lastWithMs.milestones));
  console.log('monotonic:', monotonic, '| overlay removed at', removedAt, 's | modal present at removal:', modalAtRemoval);
  await browser.close();
})();
