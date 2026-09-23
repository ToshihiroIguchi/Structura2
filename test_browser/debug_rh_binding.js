const puppeteer = require('puppeteer-core');

(async () => {
  console.log('Testing 2-latent SEM workflow (add_row first, then check x1..x3 and x4..x6)...');
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });

  const page = await browser.newPage();
  await page.setViewport({ width: 1280, height: 900 });

  await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 60000 });

  let targetFrame = null;
  for (let i = 0; i < 40; i++) {
    const frames = page.frames();
    if (frames.length > 1) {
      targetFrame = frames.find(f => f !== page.mainFrame());
      if (targetFrame) break;
    }
    await new Promise(r => setTimeout(r, 1000));
  }

  await targetFrame.waitForSelector('input[name="sample_ds"][value="HolzingerSwineford1939"]', { timeout: 90000 });
  await targetFrame.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="HolzingerSwineford1939"]');
    if (radio) radio.click();
  });

  await targetFrame.waitForSelector('#datatable', { timeout: 60000 });
  await targetFrame.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') { tab.click(); break; }
    }
  });

  await targetFrame.waitForSelector('#input_table td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('1. Clicking Add Latent Variable button...');
  await targetFrame.evaluate(() => {
    const btns = document.querySelectorAll('button, a');
    for (let b of btns) {
      if (b.innerText.includes('Add Latent Variable') || b.id === 'add_row') {
        b.click();
        break;
      }
    }
  });

  await new Promise(r => setTimeout(r, 2000));

  console.log('2. Clicking x1, x2, x3 (checkboxes 5, 6, 7) for Row 0 (LatentVariable1)...');
  for (let idx of [5, 6, 7]) {
    await targetFrame.evaluate((i) => {
      const cbs = document.querySelectorAll('#input_table tr:nth-child(1) td input[type="checkbox"]');
      if (cbs[i] && !cbs[i].disabled) cbs[i].click();
    }, idx);
    await new Promise(r => setTimeout(r, 1000));
  }

  console.log('3. Clicking x4, x5, x6 (checkboxes 8, 9, 10) for Row 1 (LatentVariable2)...');
  for (let idx of [8, 9, 10]) {
    await targetFrame.evaluate((i) => {
      const cbs = document.querySelectorAll('#input_table tr:nth-child(2) td input[type="checkbox"]');
      if (cbs[i] && !cbs[i].disabled) cbs[i].click();
    }, idx);
    await new Promise(r => setTimeout(r, 1000));
  }

  await targetFrame.waitForSelector('#checkbox_matrix td input[type="checkbox"]', { timeout: 20000 });
  await new Promise(r => setTimeout(r, 1000));

  console.log('4. Clicking structural path checkbox in checkbox_matrix...');
  await targetFrame.evaluate(() => {
    const cbs = document.querySelectorAll('#checkbox_matrix td input[type="checkbox"]');
    if (cbs[0] && !cbs[0].disabled) cbs[0].click();
  });

  await new Promise(r => setTimeout(r, 2000));

  const syntax = await targetFrame.evaluate(() => document.getElementById('lavaan_model')?.innerText);
  console.log('Generated SEM model syntax:\n', syntax);

  console.log('5. Clicking run_model button...');
  await targetFrame.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('6. Monitoring prune_model_btn visibility for up to 30s...');
  for (let s = 1; s <= 30; s++) {
    await new Promise(r => setTimeout(r, 1000));
    const btnState = await targetFrame.evaluate(() => {
      const btn = document.getElementById('prune_model_btn');
      return btn ? { display: getComputedStyle(btn).display, visible: btn.offsetWidth > 0 && btn.offsetHeight > 0 } : null;
    });
    console.log(`Sec ${s}: prune_model_btn state =`, JSON.stringify(btnState));
    if (btnState && btnState.display !== 'none') {
      console.log(`SUCCESS! prune_model_btn became visible at second ${s}!`);
      await page.screenshot({ path: 'test_browser/evidence_2latent_prune_btn.png', fullPage: true });
      break;
    }
  }

  await browser.close();
})();
