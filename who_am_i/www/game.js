/* Who Am I? — client-side game UI.
   The server owns the answers and clue order; this file renders state,
   keeps stats in localStorage and handles sharing. */
(function () {
  'use strict';

  var cfg = window.WAI || { players: [], ids: [], latest: null, url: '' };
  var STATS_KEY = 'footballGameStats';
  var THEME_KEY = 'footballGameTheme';

  var el = function (id) { return document.getElementById(id); };
  var make = function (tag, cls, text) {
    var node = document.createElement(tag);
    if (cls) node.className = cls;
    if (text !== undefined) node.textContent = text;
    return node;
  };

  /* ---------- Storage ---------- */
  function loadStats() {
    try {
      var raw = JSON.parse(localStorage.getItem(STATS_KEY));
      if (raw && typeof raw.results === 'object') return raw;
    } catch (e) { /* storage unavailable or corrupt */ }
    return { results: {}, streak: 0, best: 0 };
  }

  function saveStats() {
    try { localStorage.setItem(STATS_KEY, JSON.stringify(stats)); } catch (e) { /* ignore */ }
  }

  var stats = loadStats();
  var game = null;      // { id, total, index, over, won, history: [{kind, text}] }
  var waiting = false;  // true while a guess is with the server

  /* ---------- Talking to Shiny ---------- */
  function send(name, payload) {
    if (!window.Shiny || !Shiny.setInputValue) return false;
    payload.nonce = Date.now();
    Shiny.setInputValue(name, payload, { priority: 'event' });
    return true;
  }

  function requestPuzzle(id) {
    send('load_puzzle', { id: id });
  }

  /* ---------- Rendering ---------- */
  function setFeedback(text, kind) {
    var fb = el('feedback');
    fb.textContent = text || '';
    fb.className = 'feedback' + (kind ? ' ' + kind : '');
  }

  function setControls(enabled) {
    el('guess-input').disabled = !enabled;
    el('guess-btn').disabled = !enabled;
    el('skip-btn').disabled = !enabled;
  }

  function fillClue(li, text, extraClass) {
    li.className = 'clue' + (extraClass ? ' ' + extraClass : '');
    var body = li.querySelector('.clue-text');
    body.textContent = text;
  }

  function renderSlots(firstClue) {
    var list = el('clues');
    list.textContent = '';
    for (var i = 1; i <= game.total; i++) {
      var li = make('li', 'clue locked');
      li.id = 'clue-' + i;
      li.appendChild(make('span', 'clue-num', String(i)));
      var body = make('span', 'clue-text');
      var lock = make('i', 'fa-solid fa-lock');
      lock.setAttribute('aria-hidden', 'true');
      body.appendChild(lock);
      body.appendChild(make('span', 'visually-hidden', 'Locked clue'));
      li.appendChild(body);
      list.appendChild(li);
    }
    fillClue(el('clue-1'), firstClue, 'is-new');
  }

  function renderProgress() {
    var pips = el('pips');
    pips.textContent = '';
    for (var i = 1; i <= game.total; i++) {
      var cls = 'pip';
      var past = game.history[i - 1];
      if (past) cls += ' ' + past.kind;
      else if (i === game.index && !game.over) cls += ' current';
      pips.appendChild(make('span', cls));
    }
    el('progress-label').textContent = game.over
      ? (game.won ? 'Solved on clue ' + game.index : 'Out of clues')
      : 'Clue ' + game.index + ' of ' + game.total;

    var points = game.over && !game.won ? 0 : game.total - game.index + 1;
    var value = el('points-value');
    if (value.textContent !== String(points)) {
      value.textContent = String(points);
      value.classList.remove('bump');
      void value.offsetWidth; // restart the animation
      value.classList.add('bump');
    }
    el('points-label').textContent = game.over ? 'points scored' : 'points on offer';
  }

  function renderChips() {
    var chips = el('chips');
    chips.textContent = '';
    game.history.forEach(function (h) {
      if (h.kind === 'correct') return;
      chips.appendChild(make('span', 'chip' + (h.kind === 'skipped' ? ' skipped' : ''),
        h.kind === 'skipped' ? 'Skipped' : h.text));
    });
    chips.scrollLeft = chips.scrollWidth;
  }

  function renderStreak() {
    var pill = el('streak-pill');
    pill.hidden = !(stats.streak > 0);
    el('streak-count').textContent = String(stats.streak);
    pill.setAttribute('aria-label', 'Current streak: ' + stats.streak);
  }

  function resultGrid(g) {
    return g.history.map(function (h) {
      return h.kind === 'correct' ? '🟩' : h.kind === 'skipped' ? '⬜' : '🟥';
    }).join('');
  }

  function shareText() {
    var score = game.won ? game.index + '/' + game.total : 'X/' + game.total;
    return 'Who Am I? ⚽ #' + game.id + '\n' + resultGrid(game) + ' ' + score +
      (cfg.url ? '\n' + cfg.url : '');
  }

  function nextUnplayed() {
    var open = cfg.ids.filter(function (id) {
      return !stats.results[id] && id !== game.id;
    });
    if (!open.length) return null;
    if (open.indexOf(cfg.latest) !== -1) return cfg.latest;
    return open[Math.floor(Math.random() * open.length)];
  }

  function showEnd(answer, replay) {
    var card = el('end-card');
    var used = game.index;
    var eyebrow;
    if (!game.won) eyebrow = 'Full time';
    else if (used === 1) eyebrow = 'First clue. Unbelievable.';
    else if (used <= 3) eyebrow = 'World class';
    else if (used <= 6) eyebrow = 'Nicely done';
    else eyebrow = 'Got there in the end';

    card.className = 'end-card' + (game.won ? ' won' : '');
    el('end-eyebrow').textContent = eyebrow;
    el('end-name').textContent = answer;
    var points = game.won ? game.total - used + 1 : 0;
    el('end-meta').textContent = game.won
      ? 'Solved on clue ' + used + ' of ' + game.total + ' · ' + points + (points === 1 ? ' point' : ' points')
      : 'You used all ' + game.total + ' clues · 0 points';
    el('end-grid').textContent = resultGrid(game);
    el('end-note').textContent = replay ? 'Replay: your stats are unchanged.' : '';

    el('next-label').textContent = nextUnplayed() === null ? 'Archive' : 'Next puzzle';

    el('dock').hidden = true;
    card.hidden = false;
    card.scrollIntoView({ block: 'nearest', behavior: 'smooth' });
  }

  /* ---------- Stats ---------- */
  function recordResult() {
    if (stats.results[game.id]) return true; // replay: leave stats alone
    stats.results[game.id] = { won: game.won, used: game.index, total: game.total };
    stats.streak = game.won ? (stats.streak || 0) + 1 : 0;
    stats.best = Math.max(stats.best || 0, stats.streak);
    saveStats();
    renderStreak();
    return false;
  }

  function renderStats() {
    var ids = Object.keys(stats.results);
    var played = ids.length;
    var wins = ids.filter(function (id) { return stats.results[id].won; });
    el('stat-played').textContent = String(played);
    el('stat-win').textContent = played ? Math.round(100 * wins.length / played) + '%' : '–';
    el('stat-streak').textContent = String(stats.streak || 0);
    el('stat-best').textContent = String(stats.best || 0);

    var dist = el('dist');
    dist.textContent = '';
    if (!played) {
      dist.appendChild(make('p', 'empty-note', 'Finish a puzzle to see how many clues you needed.'));
      return;
    }
    var maxClues = 0;
    var counts = {};
    var losses = 0;
    ids.forEach(function (id) {
      var r = stats.results[id];
      maxClues = Math.max(maxClues, r.total);
      if (r.won) counts[r.used] = (counts[r.used] || 0) + 1;
      else losses++;
    });
    var peak = Math.max(losses, 1);
    Object.keys(counts).forEach(function (k) { peak = Math.max(peak, counts[k]); });

    var addRow = function (label, count, lost) {
      var row = make('div', 'dist-row');
      row.appendChild(make('span', null, label));
      var bar = make('span', 'dist-bar' + (count ? ' filled' : '') + (lost ? ' lost' : ''), String(count));
      bar.style.width = Math.max(8, Math.round(100 * count / peak)) + '%';
      row.appendChild(bar);
      dist.appendChild(row);
    };
    for (var i = 1; i <= maxClues; i++) addRow(String(i), counts[i] || 0, false);
    addRow('✕', losses, true);
  }

  function renderArchive() {
    var grid = el('archive-grid');
    grid.textContent = '';
    cfg.ids.slice().sort(function (a, b) { return b - a; }).forEach(function (id) {
      var r = stats.results[id];
      var cls = 'tile' + (r ? (r.won ? ' won' : ' lost') : '') + (game && game.id === id ? ' current' : '');
      var tile = make('button', cls, String(id));
      tile.type = 'button';
      var status = r ? (r.won ? 'solved on clue ' + r.used : 'not solved') : 'not played';
      tile.setAttribute('aria-label', 'Puzzle ' + id + ', ' + status);
      if (r) tile.appendChild(make('span', 'tile-tag', r.won ? r.used + '/' + r.total : '✕'));
      else if (id === cfg.latest) tile.appendChild(make('span', 'tile-tag', 'New'));
      tile.addEventListener('click', function () {
        el('archive-dialog').close();
        requestPuzzle(id);
      });
      grid.appendChild(tile);
    });
    var done = Object.keys(stats.results).length;
    el('archive-legend').textContent = done + ' of ' + cfg.ids.length + ' played';
  }

  /* ---------- Server messages ---------- */
  function onNewGame(msg) {
    game = { id: msg.id, total: msg.total, index: 1, over: false, won: false, history: [] };
    waiting = false;
    var label = el('puzzle-label');
    label.textContent = '';
    label.appendChild(make('span', 'label-long', 'Puzzle '));
    label.appendChild(document.createTextNode('#' + msg.id + (msg.id === cfg.latest ? ' · Latest' : ' · Archive')));
    el('end-card').hidden = true;
    el('dock').hidden = false;
    el('guess-input').value = '';
    hideSuggestions();
    setControls(true);
    renderSlots(msg.clue);
    renderProgress();
    renderChips();
    setFeedback(stats.results[msg.id] ? 'You have played this one before, so it will not change your stats.' : '');
    window.scrollTo({ top: 0, behavior: 'smooth' });
  }

  function onGuessResult(msg) {
    if (!game || msg.id !== game.id) return;
    waiting = false;
    game.history.push({ kind: msg.status, text: msg.guess || '' });

    if (!msg.over) {
      game.index = msg.index;
      var li = el('clue-' + msg.index);
      fillClue(li, msg.clue, 'is-new');
      var prev = el('clue-' + (msg.index - 1));
      if (prev) prev.classList.remove('is-new');
      li.scrollIntoView({ block: 'nearest', behavior: 'smooth' });

      var input = el('guess-input');
      input.value = '';
      setControls(true);
      var tail = msg.index === game.total ? ' Last clue!' : '';
      if (msg.status === 'wrong') {
        setFeedback('Not ' + msg.guess + '. Clue ' + msg.index + ' unlocked.' + tail, 'wrong');
        input.classList.remove('shake');
        void input.offsetWidth;
        input.classList.add('shake');
        input.focus();
      } else {
        setFeedback('Skipped. Clue ' + msg.index + ' unlocked.' + tail);
      }
      renderProgress();
      renderChips();
      return;
    }

    game.over = true;
    game.won = !!msg.won;
    (msg.rest || []).forEach(function (text, i) {
      fillClue(el('clue-' + (game.index + 1 + i)), text, 'late');
    });
    var replay = recordResult();
    renderProgress();
    renderChips();
    showEnd(msg.answer, replay);
    if (game.won) celebrate();
  }

  function celebrate() {
    if (typeof window.confetti !== 'function') return;
    if (window.matchMedia('(prefers-reduced-motion: reduce)').matches) return;
    window.confetti({
      particleCount: 120,
      spread: 75,
      origin: { y: 0.7 },
      colors: ['#0b7a4b', '#3ecf8e', '#f4c542', '#ffffff']
    });
  }

  /* ---------- Guessing ---------- */
  function submit(text) {
    if (!game || game.over || waiting) return;
    hideSuggestions();
    if (!send('guess', { text: text })) {
      setFeedback('Still connecting. Try again in a moment.');
      return;
    }
    waiting = true;
    setControls(false);
  }

  /* ---------- Autocomplete ---------- */
  var fold = function (s) {
    return s.normalize('NFD').replace(/[\u0300-\u036f]/g, '').toLowerCase().replace(/[^a-z0-9 ]/g, ' ').trim();
  };
  var index = cfg.players.map(function (name) { return { name: name, key: fold(name) }; });
  var active = -1;

  function hideSuggestions() {
    var box = el('suggestions');
    box.hidden = true;
    box.textContent = '';
    active = -1;
    el('guess-input').setAttribute('aria-expanded', 'false');
  }

  function showSuggestions() {
    var q = fold(el('guess-input').value);
    if (q.length < 2) { hideSuggestions(); return; }
    var starts = [];
    var words = [];
    index.forEach(function (p) {
      if (p.key === q) return;
      if (p.key.indexOf(q) === 0) starts.push(p.name);
      else if (p.key.indexOf(' ' + q) !== -1) words.push(p.name);
    });
    var hits = starts.concat(words).slice(0, 5);
    if (!hits.length) { hideSuggestions(); return; }

    var box = el('suggestions');
    box.textContent = '';
    active = -1;
    hits.forEach(function (name) {
      var li = make('li', null, name);
      li.setAttribute('role', 'option');
      // pointerdown fires before the input loses focus
      li.addEventListener('pointerdown', function (e) {
        e.preventDefault();
        pick(name);
      });
      box.appendChild(li);
    });
    box.hidden = false;
    el('guess-input').setAttribute('aria-expanded', 'true');
  }

  function pick(name) {
    var input = el('guess-input');
    input.value = name;
    hideSuggestions();
    input.focus();
  }

  function moveActive(step) {
    var items = el('suggestions').children;
    if (!items.length) return;
    if (active >= 0) items[active].classList.remove('active');
    active = (active + step + items.length + (active < 0 && step < 0 ? 1 : 0)) % items.length;
    items[active].classList.add('active');
  }

  /* ---------- Sharing ---------- */
  function copyText(text) {
    if (navigator.clipboard && navigator.clipboard.writeText) {
      return navigator.clipboard.writeText(text).catch(function () { return legacyCopy(text); });
    }
    return legacyCopy(text);
  }

  function legacyCopy(text) {
    return new Promise(function (resolve, reject) {
      var area = make('textarea');
      area.value = text;
      area.setAttribute('readonly', '');
      area.style.position = 'fixed';
      area.style.opacity = '0';
      document.body.appendChild(area);
      area.select();
      var ok = false;
      try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
      document.body.removeChild(area);
      if (ok) resolve(); else reject(new Error('copy failed'));
    });
  }

  function share() {
    var label = el('share-label');
    var text = shareText();
    var flash = function (msg) {
      label.textContent = msg;
      setTimeout(function () { label.textContent = 'Share'; }, 2000);
    };
    var touch = window.matchMedia('(pointer: coarse)').matches;
    var copy = function () {
      copyText(text).then(function () { flash('Copied!'); }, function () { flash('Copy failed'); });
    };
    if (touch && navigator.share) {
      navigator.share({ text: text }).catch(function (err) {
        if (err && err.name === 'AbortError') return; // user closed the share sheet
        copy();
      });
    } else {
      copy();
    }
  }

  /* ---------- Wiring ---------- */
  function openDialog(id, before) {
    if (before) before();
    var dialog = el(id);
    if (typeof dialog.showModal === 'function') dialog.showModal();
    else dialog.setAttribute('open', '');
  }

  function init() {
    var input = el('guess-input');

    el('guess-form').addEventListener('submit', function (e) {
      e.preventDefault();
      var text = input.value.trim();
      if (!text) { input.focus(); return; }
      submit(text);
    });
    el('skip-btn').addEventListener('click', function () { submit(''); });

    input.addEventListener('input', showSuggestions);
    input.addEventListener('blur', hideSuggestions);
    input.addEventListener('animationend', function () { input.classList.remove('shake'); });
    input.addEventListener('keydown', function (e) {
      var open = !el('suggestions').hidden;
      if (e.key === 'ArrowDown' && open) { e.preventDefault(); moveActive(1); }
      else if (e.key === 'ArrowUp' && open) { e.preventDefault(); moveActive(-1); }
      else if (e.key === 'Escape' && open) { e.preventDefault(); hideSuggestions(); }
      else if (e.key === 'Enter' && open && active >= 0) {
        e.preventDefault();
        pick(el('suggestions').children[active].textContent);
      }
    });

    el('theme-btn').addEventListener('click', function () {
      var next = document.documentElement.getAttribute('data-theme') === 'dark' ? 'light' : 'dark';
      document.documentElement.setAttribute('data-theme', next);
      try { localStorage.setItem(THEME_KEY, next); } catch (e) { /* ignore */ }
    });

    el('stats-btn').addEventListener('click', function () { openDialog('stats-dialog', renderStats); });
    el('streak-pill').addEventListener('click', function () { openDialog('stats-dialog', renderStats); });
    el('archive-btn').addEventListener('click', function () { openDialog('archive-dialog', renderArchive); });
    el('help-btn').addEventListener('click', function () { openDialog('help-dialog'); });

    document.querySelectorAll('dialog.sheet').forEach(function (dialog) {
      dialog.addEventListener('click', function (e) {
        if (e.target === dialog || e.target.closest('[data-close]')) dialog.close();
      });
    });

    el('share-btn').addEventListener('click', share);
    el('next-btn').addEventListener('click', function () {
      var id = nextUnplayed();
      if (id === null) openDialog('archive-dialog', renderArchive);
      else requestPuzzle(id);
    });

    renderStreak();
  }

  init();

  Shiny.addCustomMessageHandler('new-game', onNewGame);
  Shiny.addCustomMessageHandler('guess-result', onGuessResult);
  // jQuery event: fires once the session is ready to receive input values
  window.jQuery(document).on('shiny:sessioninitialized', function () { requestPuzzle(null); });
})();
