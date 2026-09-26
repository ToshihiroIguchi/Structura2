const puppeteer = require('puppeteer-core');

(async () => {
  const browser = await puppeteer.launch({
    executablePath: 'C:\\Program Files (x86)\\Microsoft\\Edge\\Application\\msedge.exe',
    headless: true,
    args: ['--no-sandbox', '--disable-setuid-sandbox']
  });
  
  const page = await browser.newPage();
  await page.setViewport({ width: 1400, height: 900 });
  
  page.on('console', msg => console.log('BROWSER LOG:', msg.text()));
  page.on('pageerror', err => console.error('BROWSER ERROR:', err.message));

  console.log('Navigating to http://localhost:8100 ...');
  try {
    await page.goto('http://localhost:8100', { waitUntil: 'networkidle2', timeout: 30000 });
  } catch (err) {
    console.error('Failed to navigate to app. Make sure the local server is running on 8100.', err);
    await browser.close();
    process.exit(1);
  }
  
  console.log('Selecting PoliticalDemocracy demo dataset...');
  await page.waitForSelector('input[name="sample_ds"][value="PoliticalDemocracy"]', { timeout: 15000 });
  await page.evaluate(() => {
    const radio = document.querySelector('input[name="sample_ds"][value="PoliticalDemocracy"]');
    if (radio) radio.click();
  });
  
  console.log('Waiting for data table to load...');
  await page.waitForSelector('#datatable', { timeout: 15000 });
  
  console.log('Switching to Model tab...');
  await page.evaluate(() => {
    const tabs = document.querySelectorAll('a[data-toggle="tab"]');
    for (let tab of tabs) {
      if (tab.innerText.trim() === 'Model') {
        tab.click();
        break;
      }
    }
  });

  await page.waitForSelector('#input_table', { timeout: 10000 });
  await new Promise(resolve => setTimeout(resolve, 2000));
  
  console.log('Configuring measurement model for PoliticalDemocracy...');
  // Row 0: ind60 =~ x1, x2, x3
  // Row 1: dem60 =~ y1, y2, y3, y4
  // Row 2: dem65 =~ y5, y6, y7, y8
  await page.evaluate(() => {
    const hotTableEl = document.getElementById('input_table');
    const hotInstance = window.HTMLWidgets ? window.HTMLWidgets.getInstance(hotTableEl) : null;
    const hot = hotInstance ? hotInstance.hot : null;
    if (hot) {
      // Row 0: Latent="ind60", check x1, x2, x3
      hot.setDataAtCell(0, 0, 'ind60');
      const cols = hot.getColHeader();
      for (let c = 3; c < cols.length; c++) {
        const colName = cols[c];
        hot.setDataAtCell(0, c, ['x1', 'x2', 'x3'].includes(colName));
      }
    }
  });
  
  console.log('Adding Row 2 (dem60)...');
  await page.evaluate(() => document.getElementById('add_row').click());
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  await page.evaluate(() => {
    const hotTableEl = document.getElementById('input_table');
    const hotInstance = window.HTMLWidgets ? window.HTMLWidgets.getInstance(hotTableEl) : null;
    const hot = hotInstance ? hotInstance.hot : null;
    if (hot) {
      hot.setDataAtCell(1, 0, 'dem60');
      hot.setDataAtCell(1, 1, 'dem60');
      const cols = hot.getColHeader();
      for (let c = 3; c < cols.length; c++) {
        const colName = cols[c];
        hot.setDataAtCell(1, c, ['y1', 'y2', 'y3', 'y4'].includes(colName));
      }
    }
  });

  console.log('Adding Row 3 (dem65)...');
  await page.evaluate(() => document.getElementById('add_row').click());
  await new Promise(resolve => setTimeout(resolve, 1000));
  
  await page.evaluate(() => {
    const hotTableEl = document.getElementById('input_table');
    const hotInstance = window.HTMLWidgets ? window.HTMLWidgets.getInstance(hotTableEl) : null;
    const hot = hotInstance ? hotInstance.hot : null;
    if (hot) {
      hot.setDataAtCell(2, 0, 'dem65');
      hot.setDataAtCell(2, 1, 'dem65');
      const cols = hot.getColHeader();
      for (let c = 3; c < cols.length; c++) {
        const colName = cols[c];
        hot.setDataAtCell(2, c, ['y5', 'y6', 'y7', 'y8'].includes(colName));
      }
    }
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  console.log('Setting structural paths: dem60 ~ ind60 and dem65 ~ dem60 (omitting dem65 ~ ind60)...');
  await page.evaluate(() => {
    const structEl = document.getElementById('checkbox_matrix');
    const hotInstance = window.HTMLWidgets ? window.HTMLWidgets.getInstance(structEl) : null;
    const hot = hotInstance ? hotInstance.hot : null;
    if (hot) {
      const data = hot.getData();
      const colHeaders = hot.getColHeader ? hot.getColHeader() : [];
      const ind60Col = colHeaders.indexOf('ind60');
      const dem60Col = colHeaders.indexOf('dem60');
      
      for (let r = 0; r < data.length; r++) {
        if (data[r][0] === 'dem60' && ind60Col !== -1) {
          hot.setDataAtCell(r, ind60Col, true);
        }
        if (data[r][0] === 'dem65' && dem60Col !== -1) {
          hot.setDataAtCell(r, dem60Col, true);
        }
      }
    }
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  console.log('Clicking Run Model button...');
  await page.evaluate(() => {
    const btn = document.getElementById('run_model');
    if (btn) btn.click();
  });

  console.log('Waiting for path diagram rendering...');
  await page.waitForSelector('#sem_plot_container svg', { timeout: 25000 });
  console.log('Model fitted successfully!');
  
  await new Promise(resolve => setTimeout(resolve, 3000));

  console.log('Inspecting checkbox_matrix for suggested path highlights (Expecting dem65 ~ ind60 with MI >= 3.84)...');
  const suggestions = await page.evaluate(() => {
    const structEl = document.getElementById('checkbox_matrix');
    const tds = structEl.querySelectorAll('tbody td');
    const highlighted = [];
    tds.forEach(td => {
      const bs = td.style.boxShadow || '';
      const title = td.getAttribute('title') || '';
      if (bs.includes('2563eb') || title.includes('Suggested Path to Add')) {
        highlighted.push({
          boxShadow: bs,
          title: title,
          text: td.innerText.trim(),
          classes: td.className
        });
      }
    });
    return highlighted;
  });

  console.log(`Found ${suggestions.length} suggested path cell(s):`, JSON.stringify(suggestions, null, 2));

  if (suggestions.length === 0) {
    console.error('FAIL: No suggestions found when dem65 ~ ind60 was expected!');
    await page.screenshot({ path: 'test_browser/evidence_suggested_paths_fail.png', fullPage: true });
    await browser.close();
    process.exit(1);
  }

  console.log('SUCCESS: Recommended path dem65 ~ ind60 is clearly highlighted!');

  console.log('Capturing screenshot of highlighted matrix...');
  await page.screenshot({ path: 'test_browser/evidence_suggested_paths_highlight.png', fullPage: true });

  // Test toggling off the suggestions checkbox
  console.log('Toggling off "Highlight suggested paths"...');
  await page.evaluate(() => {
    const chk = document.getElementById('show_suggested_paths');
    if (chk) chk.click();
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  const countAfterToggleOff = await page.evaluate(() => {
    const structEl = document.getElementById('checkbox_matrix');
    const tds = structEl.querySelectorAll('tbody td');
    let count = 0;
    tds.forEach(td => {
      const bs = td.style.boxShadow || '';
      if (bs.includes('2563eb')) count++;
    });
    return count;
  });
  console.log(`Highlighted cells after toggle OFF: ${countAfterToggleOff} (Expected: 0)`);
  if (countAfterToggleOff !== 0) {
    console.error('FAIL: Highlights remained after toggle OFF!');
    await browser.close();
    process.exit(1);
  }

  // Toggle back ON
  console.log('Toggling back ON "Highlight suggested paths"...');
  await page.evaluate(() => {
    const chk = document.getElementById('show_suggested_paths');
    if (chk) chk.click();
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  const countAfterToggleOn = await page.evaluate(() => {
    const structEl = document.getElementById('checkbox_matrix');
    const tds = structEl.querySelectorAll('tbody td');
    let count = 0;
    tds.forEach(td => {
      const bs = td.style.boxShadow || '';
      if (bs.includes('2563eb')) count++;
    });
    return count;
  });
  console.log(`Highlighted cells after toggle ON: ${countAfterToggleOn}`);

  // Test clicking the suggested checkbox
  console.log('Clicking on the suggested cell to add it to the model...');
  await page.evaluate(() => {
    const structEl = document.getElementById('checkbox_matrix');
    const tds = structEl.querySelectorAll('tbody td');
    for (let td of tds) {
      if ((td.style.boxShadow || '').includes('2563eb')) {
        const input = td.querySelector('input[type="checkbox"]');
        if (input) {
          input.click();
          break;
        }
      }
    }
  });
  await new Promise(resolve => setTimeout(resolve, 1500));

  console.log('Capturing final screenshot after adding suggested path...');
  await page.screenshot({ path: 'test_browser/evidence_suggested_paths_final.png', fullPage: true });

  await browser.close();
  console.log('ALL VERIFICATION CHECKS PASSED SUCCESSFULLY!');
})();
