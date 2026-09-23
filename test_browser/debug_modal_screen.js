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

  console.log('Navigating to http://localhost:8100 ...');
  await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 45000 });
  
  console.log('Waiting 8 seconds for app startup...');
  await new Promise(resolve => setTimeout(resolve, 8000));
  
  await page.screenshot({ path: 'test_browser/debug_startup_screen.png' });
  console.log('Startup screenshot saved to test_browser/debug_startup_screen.png');
  
  const modalText = await page.evaluate(() => {
    const modal = document.querySelector('.modal-content');
    return modal ? modal.innerText : 'NO MODAL FOUND';
  });
  console.log('Modal text:', modalText);
  
  await browser.close();
})();
