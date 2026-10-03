(function () {
  var startTime = Date.now();
  var dismissed = false;
  var failed = false;
  var slowDismissed = false;

  // Ordered startup milestones. `pct` is the bar position once the milestone is reached; `doing`
  // is shown while waiting for the next milestone and `tau` is the expected wait in seconds, used
  // only to ease the bar toward (never onto) the next milestone.
  var MILESTONES = [
    { id: 'page',          pct: 2,   doing: 'Preparing R runtime...',                  tau: 1 },
    { id: 'worker',        pct: 8,   doing: 'Downloading R runtime (WebAssembly)...',  tau: 3 },
    { id: 'runtime',       pct: 25,  doing: 'Loading R packages...',                   tau: 6 },
    { id: 'app_start',     pct: 60,  doing: 'Starting Structura2...',                  tau: 1 },
    { id: 'libs_attached', pct: 66,  doing: 'Loading SEM engine (lavaan)...',          tau: 3 },
    { id: 'ui_built',      pct: 88,  doing: 'Preparing interface...',                  tau: 2 },
    { id: 'session_start', pct: 95,  doing: 'Finalizing...',                           tau: 1 },
    { id: 'ready',         pct: 100, doing: 'Ready!',                                  tau: 1 }
  ];
  // User-visible checklist; each row completes when its milestone is reached.
  var CHECKLIST = [
    { label: 'Download R runtime',       doneAt: 'runtime' },
    { label: 'Load R packages',          doneAt: 'app_start' },
    { label: 'Load SEM engine (lavaan)', doneAt: 'ui_built' },
    { label: 'Start interface',          doneAt: 'ready' }
  ];

  var current = -1;          // index of the last reached milestone
  var reachedAt = Date.now();
  var shownPct = 0;
  var times = {};            // milestone id -> ms since start (exposed for profiling)
  window.__structuraMilestones = times;

  var fillElem = document.getElementById('structura-progress-fill');
  var statusElem = document.getElementById('structura-splash-status');
  var metaElem = document.getElementById('structura-splash-meta');
  var listElem = document.getElementById('structura-stage-list');

  function indexOfId(id) {
    for (var i = 0; i < MILESTONES.length; i++) { if (MILESTONES[i].id === id) return i; }
    return -1;
  }

  function hideOverlay() {
    if (dismissed) return;
    dismissed = true;
    clearInterval(tickTimer);
    clearInterval(domCheck);
    var overlay = document.getElementById('structura-splash-overlay');
    if (overlay) {
      overlay.style.transition = 'opacity 0.6s ease-out';
      overlay.style.opacity = '0';
      setTimeout(function () { overlay.remove(); }, 650);
    }
  }
  window.__hideStructuraOverlay = hideOverlay;

  function renderChecklist() {
    if (!listElem) return;
    var html = '';
    var activeFound = false;
    for (var i = 0; i < CHECKLIST.length; i++) {
      var done = current >= indexOfId(CHECKLIST[i].doneAt);
      var cls = 'structura-stage-pending', mark = '&#9675;';
      if (done) { cls = 'structura-stage-done'; mark = '&#10003;'; }
      else if (!activeFound) { cls = 'structura-stage-active'; mark = '&#9654;'; activeFound = true; }
      html += '<li class="' + cls + '"><span class="structura-stage-mark">' + mark + '</span>' +
              CHECKLIST[i].label + '</li>';
    }
    listElem.innerHTML = html;
  }

  function reach(id) {
    if (dismissed || failed) return;
    var idx = indexOfId(id);
    if (idx <= current) return;
    for (var i = current + 1; i <= idx; i++) { times[MILESTONES[i].id] = Date.now() - startTime; }
    current = idx;
    reachedAt = Date.now();
    renderChecklist();
    render();
    if (id === 'ready') { setTimeout(hideOverlay, 400); }
  }

  function render() {
    if (dismissed || failed) return;
    var base = current >= 0 ? MILESTONES[current].pct : 0;
    var pct = base;
    if (current < MILESTONES.length - 1) {
      var next = MILESTONES[current + 1];
      var tau = current >= 0 ? MILESTONES[current].tau : 1;
      var since = (Date.now() - reachedAt) / 1000;
      // Ease toward, but never reach, the next milestone.
      pct = base + (next.pct - base) * 0.9 * (1 - Math.exp(-since / tau));
    }
    if (pct > shownPct) shownPct = pct;
    if (fillElem) fillElem.style.width = shownPct.toFixed(1) + '%';
    if (statusElem && !failed) {
      statusElem.textContent = current >= 0 ? MILESTONES[current].doing : 'Starting...';
    }
    var elapsed = Math.floor((Date.now() - startTime) / 1000);
    if (metaElem) metaElem.textContent = Math.floor(shownPct) + '% · ' + elapsed + ' s';
    if (!slowDismissed && !failed && elapsed >= 45) {
      var slow = document.getElementById('structura-splash-slow');
      if (slow) slow.hidden = false;
    }
  }

  function showError(message) {
    if (dismissed || failed) return;
    failed = true;
    if (fillElem) { fillElem.classList.add('structura-failed'); fillElem.style.width = '100%'; }
    if (statusElem) statusElem.textContent = 'Startup failed';
    var slow = document.getElementById('structura-splash-slow');
    if (slow) slow.hidden = true;
    var errBox = document.getElementById('structura-splash-error');
    var errText = document.getElementById('structura-error-text');
    if (errText) errText.textContent = message || 'unknown error';
    if (errBox) errBox.hidden = false;
  }

  function reload() { window.location.reload(); }
  var waitBtn = document.getElementById('structura-slow-wait');
  if (waitBtn) waitBtn.onclick = function () {
    slowDismissed = true;
    document.getElementById('structura-splash-slow').hidden = true;
  };
  var reloadBtn = document.getElementById('structura-slow-reload');
  if (reloadBtn) reloadBtn.onclick = reload;
  var errReloadBtn = document.getElementById('structura-error-reload');
  if (errReloadBtn) errReloadBtn.onclick = reload;

  // 1. Observe the WebR worker created by shinylive.js (this script runs before it).
  try {
    var OrigWorker = window.Worker;
    var HookedWorker = function (url, opts) {
      var w = new OrigWorker(url, opts);
      if (String(url).indexOf('webr-worker') !== -1) {
        reach('worker');
        var runtimeSeen = false;
        w.addEventListener('message', function (e) {
          var d = e.data;
          if (!runtimeSeen && d && d.type === 'prompt') { runtimeSeen = true; reach('runtime'); }
        });
        w.addEventListener('error', function () {
          showError('The R runtime could not be loaded. Check your network connection and reload.');
        });
      }
      return w;
    };
    HookedWorker.prototype = OrigWorker.prototype;
    window.Worker = HookedWorker;
  } catch (e) { /* progress display is best-effort */ }

  // 2. Milestones reported by app.R through a BroadcastChannel.
  try {
    if (typeof BroadcastChannel !== 'undefined') {
      var channel = new BroadcastChannel('structura-progress');
      channel.onmessage = function (e) {
        if (e.data && e.data.type === 'stage') reach(e.data.stage);
      };
    }
  } catch (e) { /* best-effort */ }

  // 3. Ready / error signals posted by the Shiny server from inside the app iframe.
  window.addEventListener('message', function (e) {
    var d = e.data;
    if (d && (d.type === 'structura-ready' || d === 'structura-ready')) reach('ready');
    else if (d && d.type === 'structura-error') showError(d.message);
  });

  // 4. Fallback: the Load Data modal (not the merely-existing hidden app container) is visible.
  var domCheck = setInterval(function () {
    if (dismissed) return;
    try {
      var iframe = document.querySelector('iframe');
      var doc = iframe && iframe.contentDocument;
      if (doc && doc.querySelector('.modal-dialog, #sample_ds')) reach('ready');
    } catch (e) { /* cross-origin or not ready yet */ }
  }, 500);

  var tickTimer = setInterval(render, 250);
  renderChecklist();
  reach('page');
})();
