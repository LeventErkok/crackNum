// crackNum web GUI.
//
// This file builds an argv-shaped request and renders whatever text comes back.
// It deliberately knows nothing about floating point: it never parses, reformats
// or second-guesses crackNum's output, because the moment it does there are two
// implementations to keep in sync. The <pre> shows exactly what the CLI printed.

'use strict';

// Mirrors Model.welcome in the Swift GUI.
var WELCOME = [
  'Enter a value above, then pick a format on the left to crack it.',
  '',
  'You can:',
  '  - ENCODE: from a mathematical value to its internal representation',
  '  - DECODE: from an internal representation to its mathematical value',
  '',
  'Encoding:',
  '  - Enter a decimal value (2.5, -4.1e5) or hex float (0x2.4p3).',
  '  - You can pass NaN, Inf, -0, -Inf for special values.',
  '  - For floats, pick a rounding mode.',
  '  - Input must NOT start with 0x, 0b, or N\'h (else we decode instead).',
  '',
  'Decoding:',
  '  - Use hex (0x), binary (0b), or Verilog (N\'h) notation.',
  '  - You may use _, - or space as separators for readability.',
  '  - Verilog N\'h: N is the total width, split into N/format-size lanes.'
].join('\n');

var el = {
  value:    document.getElementById('value'),
  help:     document.getElementById('help'),
  sections: document.getElementById('sections'),
  rounding: document.getElementById('rounding'),
  heading:  document.getElementById('custom-heading'),
  width:      document.getElementById('width'),
  exp:        document.getElementById('exp'),
  widthLabel: document.getElementById('width-label'),
  expLabel:   document.getElementById('exp-label'),
  output:    document.getElementById('output'),
  copy:      document.getElementById('copy'),
  permalink: document.getElementById('permalink'),
  toast:     document.getElementById('toast')
};

var formats = {};        // id -> {id, label, kind}
var selection = null;    // format id, or null
var seq = 0;             // request counter, so a slow reply cannot overwrite a fast one

// ------------------------------------------------------------------ helpers

function toast(msg) {
  el.toast.textContent = msg;
  setTimeout(function () {
    if (el.toast.textContent === msg) el.toast.textContent = '';
  }, 1600);
}

// What the width boxes are currently driving. Mirrors `customBox` in the Swift
// GUI: the boxes are shared by all three "Custom" entries, but only the float
// has an exponent, and with a fixed format selected neither box does anything.
function customBox() {
  var kind = selection && formats[selection] ? formats[selection].kind : null;
  if (kind === 'customFloat') return ['Custom IEEE-754 float:', true, true];
  if (kind === 'customInt')   return ['Custom signed integer:', true, false];
  if (kind === 'customWord')  return ['Custom unsigned word:',  true, false];
  return ['Custom format:', false, false];
}

function syncCustomBox() {
  var box = customBox(), heading = box[0], widthOn = box[1], expOn = box[2];
  el.heading.textContent = heading;
  el.width.disabled = !widthOn;
  el.exp.disabled = !expOn;
  el.widthLabel.classList.toggle('off', !widthOn);
  el.expLabel.classList.toggle('off', !expOn);
}

// ------------------------------------------------------------- permalinks

function readURL() {
  var q = new URLSearchParams(window.location.search);
  if (q.has('v')) el.value.value = q.get('v');
  if (q.has('w')) el.width.value = q.get('w');
  if (q.has('e')) el.exp.value = q.get('e');
  return { fmt: q.get('f'), rounding: q.get('r') };
}

function permalinkURL() {
  var q = new URLSearchParams();
  if (selection) q.set('f', selection);
  if (el.value.value) q.set('v', el.value.value);
  if (el.rounding.value && el.rounding.value !== 'RNE') q.set('r', el.rounding.value);
  var box = customBox();
  if (box[1]) q.set('w', el.width.value);
  if (box[2]) q.set('e', el.exp.value);
  var qs = q.toString();
  return window.location.origin + window.location.pathname + (qs ? '?' + qs : '');
}

function syncURL() {
  window.history.replaceState(null, '', permalinkURL());
}

// -------------------------------------------------------------------- run

function run() {
  syncCustomBox();
  if (!selection) { el.output.textContent = WELCOME; syncURL(); return; }
  syncURL();

  var mine = ++seq;
  el.output.classList.add('busy');

  fetch('api/crack', {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({
      format:   selection,
      rounding: el.rounding.value,
      value:    el.value.value,
      width:    el.width.value,
      exp:      el.exp.value
    })
  }).then(function (r) {
    return r.json();
  }).then(function (data) {
    if (mine !== seq) return;            // a newer request is already in flight
    el.output.classList.remove('busy');
    el.output.textContent = data.text || '';
  }).catch(function (err) {
    if (mine !== seq) return;
    el.output.classList.remove('busy');
    el.output.textContent =
      'Could not reach the crackNum service.\n\n' + err + '\n\n' +
      'Is the server still running?';
  });
}

function select(id) {
  selection = id;
  Array.prototype.forEach.call(el.sections.querySelectorAll('.fmt'), function (b) {
    b.setAttribute('aria-pressed', String(b.dataset.id === id));
  });
  run();
}

// ------------------------------------------------------------------- build

function build(payload, wanted) {
  payload.sections.forEach(function (section) {
    var h = document.createElement('div');
    h.className = 'heading';
    h.textContent = section.title;
    el.sections.appendChild(h);

    section.formats.forEach(function (f) {
      formats[f.id] = f;
      var b = document.createElement('button');
      b.type = 'button';
      b.className = 'fmt';
      b.dataset.id = f.id;
      b.textContent = f.label;
      b.setAttribute('aria-pressed', 'false');
      b.addEventListener('click', function () { select(f.id); });
      el.sections.appendChild(b);
    });
  });

  payload.roundingModes.forEach(function (rm) {
    var o = document.createElement('option');
    o.value = rm.id;
    o.textContent = rm.label;
    el.rounding.appendChild(o);
  });

  if (wanted.rounding && formats) el.rounding.value = wanted.rounding;
  if (!el.rounding.value) el.rounding.value = 'RNE';

  if (wanted.fmt && formats[wanted.fmt]) {
    select(wanted.fmt);                  // a permalink cracks on arrival
  } else {
    el.output.textContent = WELCOME;
    syncCustomBox();
  }
}

// ------------------------------------------------------------------ events

el.value.addEventListener('keydown', function (e) {
  if (e.key === 'Enter') { e.preventDefault(); run(); }
});
el.value.addEventListener('change', run);
el.rounding.addEventListener('change', run);
el.width.addEventListener('change', run);
el.exp.addEventListener('change', run);

el.help.addEventListener('click', function () { el.output.textContent = WELCOME; });

// navigator.clipboard exists only in a secure context -- https, or localhost.
// Served over plain http from a hostname it is undefined, and reading
// .writeText off it throws before any promise is created, so both buttons
// would silently do nothing. Fall back to a scratch textarea in that case.
function copy(text, done) {
  if (window.isSecureContext && navigator.clipboard) {
    navigator.clipboard.writeText(text).then(
      function () { toast(done); },
      function () { toast('Copy failed.'); }
    );
    return;
  }

  var ta = document.createElement('textarea');
  ta.value = text;
  ta.setAttribute('readonly', '');
  ta.style.position = 'fixed';   // don't scroll the page to it
  ta.style.top = '-1000px';
  document.body.appendChild(ta);
  ta.select();
  ta.setSelectionRange(0, ta.value.length);   // iOS wants the explicit range
  var ok = false;
  try { ok = document.execCommand('copy'); } catch (e) { ok = false; }
  document.body.removeChild(ta);
  toast(ok ? done : 'Copy failed -- select the text and copy by hand.');
}

el.copy.addEventListener('click', function () {
  copy(el.output.textContent, 'Output copied.');
});

el.permalink.addEventListener('click', function () {
  copy(permalinkURL(), 'Link copied.');
});

// -------------------------------------------------------------------- boot

var wanted = readURL();
el.output.textContent = WELCOME;

fetch('api/formats').then(function (r) {
  return r.json();
}).then(function (payload) {
  build(payload, wanted);
}).catch(function (err) {
  el.output.textContent = 'Could not load the format list.\n\n' + err;
});
