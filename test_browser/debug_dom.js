const puppeteer = require('puppeteer-core');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    viewport: { width: 1400, height: 950 },
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));

  await page.goto('http://localhost:8080', { waitUntil: 'networkidle2', timeout: 45000 });
  await new Promise(r => setTimeout(r, 15000));
  
  const info = await page.evaluate(() => {
    const inputs = Array.from(document.querySelectorAll('input')).map(i => ({ type: i.type, name: i.name, value: i.value, id: i.id }));
    const modal = document.querySelector('.modal-body');
    return { inputs, modalText: modal ? modal.innerText : 'NO MODAL' };
  });
  
  console.log('DOM INPUTS:', JSON.stringify(info, null, 2));
  await page.screenshot({ path: 'test_browser/debug_inputs.png' });
  await browser.close();
})();
