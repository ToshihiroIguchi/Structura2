const fs = require('fs');
const js = fs.readFileSync('site/shinylive-sw.js', 'utf8');
const idx = js.indexOf("addEventListener('fetch'");
const idx2 = js.indexOf('addEventListener("fetch"');
const pos = idx !== -1 ? idx : idx2;
if (pos !== -1) {
  console.log(js.substring(pos - 100, pos + 1000));
} else {
  console.log('Not found addEventListener fetch');
}
