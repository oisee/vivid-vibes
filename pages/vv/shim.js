// Vivid Vibes on GitHub Pages: what stands in for the SAP system under the
// unmodified player page. Loaded first in <head> by every player.
//
// - WebSocket: a socket to /sap/bc/apc/sap/zo4d_demo is answered in the page
//   by a Web Worker instead of by APC. One worker per socket, because the
//   demo's registry is class data and the dev player's PREFETCH opens several
//   sockets at once. Two kinds of worker, one protocol:
//     live      vv/live-worker.js: the demo's ABAP compiled to JS by gogen
//     recorded  vv/replay-worker.js: frames recorded from the Go build
// - Media: the handler's ?audio= / ?img= / ?image= URLs map to media/.
// - Mode: live by default; recorded when the device cannot keep the frame
//   rate (a probe of a heavy scene plus the first frames played), or when
//   the switch in the corner says so. ?mode=live|recorded pins it.
(function () {
  "use strict";
  var here = new URL(".", location.href);
  var page = location.pathname.split("/").pop() || "index.html";
  var params = new URLSearchParams(location.search);

  // ---- the handler's routes: ?player=dev&demo=x is a page of its own here
  var player = params.get("player");
  var demoParam = (params.get("demo") || "main").replace(/[^a-z0-9_]/gi, "");
  if (player === "dev" && !/^dev-/.test(page)) { location.replace(new URL("dev-" + demoParam + ".html", here).href); return; }
  if (player === "megademo" && page !== "index.html") { location.replace(here.href); return; }

  // ---- media
  function mapUrl(u) {
    try {
      var url = new URL(String(u), location.href);
      if (url.origin !== location.origin) return u;
      var q = url.search.slice(1), m;
      if ((m = /^(?:img|image)=([^&]+)/i.exec(q))) {
        var n = decodeURIComponent(m[1]).toLowerCase();
        return new URL("media/" + (/\.png$/.test(n) ? n : n + ".png"), here).href;
      }
      if ((m = /^audio=([^&]+)/i.exec(q))) return new URL("media/" + decodeURIComponent(m[1]).toLowerCase(), here).href;
    } catch (e) { /* not a URL: leave it */ }
    return u;
  }
  [window.HTMLImageElement, window.HTMLMediaElement, window.HTMLSourceElement].forEach(function (C) {
    var d = C && Object.getOwnPropertyDescriptor(C.prototype, "src");
    if (!d || !d.set) return;
    Object.defineProperty(C.prototype, "src", {configurable: true, enumerable: d.enumerable, get: d.get,
      set: function (v) { d.set.call(this, mapUrl(v)); }});
  });
  var setAttr = Element.prototype.setAttribute;
  Element.prototype.setAttribute = function (k, v) {
    if (String(k).toLowerCase() === "src" && /^(IMG|AUDIO|VIDEO|SOURCE)$/.test(this.tagName)) v = mapUrl(v);
    return setAttr.call(this, k, v);
  };
  // <source src="?audio=..."> written in the page itself, as the parser meets it
  function fixParsed(el) {
    if (!el.getAttribute || !/^(IMG|AUDIO|VIDEO|SOURCE)$/.test(el.tagName)) return false;
    var v = el.getAttribute("src");
    if (!v || mapUrl(v) === v) return false;
    setAttr.call(el, "src", mapUrl(v));
    return true;
  }
  new MutationObserver(function (list) {
    list.forEach(function (r) {
      r.addedNodes.forEach(function (n) {
        if (fixParsed(n) && n.tagName === "SOURCE" && n.parentNode && n.parentNode.load) n.parentNode.load();
      });
    });
  }).observe(document.documentElement, {childList: true, subtree: true});

  // ---- modes
  var VV = window.VV = {
    mode: "live", auto: true, recorded: null, sockets: [], stats: {frames: 0, ms: [], dropped: 0},
    listeners: [],
  };
  var pinned = params.get("mode");
  if (pinned === "live" || pinned === "recorded") { VV.mode = pinned; VV.auto = false; }
  var recBase = new URL(params.get("rec") || "rec/", here).href;
  VV.recBase = recBase;
  VV.recReady = fetch(recBase + "index.json", {cache: "no-cache"}).then(function (r) {
    if (!r.ok) throw new Error(r.status);
    return r.json();
  }).then(function (j) { VV.recorded = j; changed(); return true; }, function () {
    VV.recorded = false;
    if (VV.mode === "recorded") setMode("live", "recorded frames are not published here");
    changed();
    return false;
  });
  function changed() { VV.listeners.forEach(function (f) { try { f(); } catch (e) { /* ui */ } }); }

  function setMode(mode, why) {
    if (mode === "recorded" && VV.recorded === false) return;
    if (VV.mode === mode) { changed(); return; }
    VV.mode = mode;
    VV.why = why || "";
    VV.stats = {frames: 0, ms: [], dropped: 0};
    VV.sockets.forEach(function (s) { s._switch(); });
    changed();
  }
  VV.setMode = function (mode) { VV.auto = false; setMode(mode, "chosen"); };

  // ---- the socket
  var CHANNEL = /\/sap\/bc\/apc\/sap\/zo4d_demo\/?(\?|$)/;
  var NativeWebSocket = window.WebSocket;
  var FRAME = /^\{"cmd":"frame"/;
  var seq = 0;

  function VVSocket(url, protocols) {
    if (!CHANNEL.test(String(url))) return new NativeWebSocket(url, protocols);
    var t = new EventTarget();
    Object.setPrototypeOf(t, VVSocket.prototype);
    t.url = String(url);
    t.readyState = 0;
    t.protocol = "";
    t.extensions = "";
    t.bufferedAmount = 0;
    t.binaryType = "blob";
    t._queue = [];
    t._busy = false;
    t._state = []; // commands that set state, replayed when the worker is swapped
    t._id = ++seq;
    VV.sockets.push(t);
    t._start(false);
    return t;
  }
  VVSocket.prototype = Object.create(EventTarget.prototype);
  VVSocket.prototype.constructor = VVSocket;
  ["CONNECTING", "OPEN", "CLOSING", "CLOSED"].forEach(function (k, i) { VVSocket[k] = i; VVSocket.prototype[k] = i; });
  ["open", "message", "close", "error"].forEach(function (k) {
    var slot = "_on" + k;
    Object.defineProperty(VVSocket.prototype, "on" + k, {
      get: function () { return this[slot] || null; },
      set: function (f) {
        if (this[slot]) this.removeEventListener(k, this[slot]);
        this[slot] = typeof f === "function" ? f : null;
        if (this[slot]) this.addEventListener(k, this[slot]);
      },
    });
  });

  VVSocket.prototype._start = function (silent) {
    var self = this;
    var kind = VV.mode === "recorded" ? "replay" : "live";
    var w = new Worker(new URL("vv/" + kind + "-worker.js", here), {type: "module"});
    self._worker = w;
    self._kind = kind;
    self._busy = true; // until the worker says open
    w.onmessage = function (e) {
      if (self._worker !== w) return;
      var d = e.data;
      if (d.op === "open") {
        self._busy = false;
        if (!silent) { self.readyState = 1; self.dispatchEvent(new Event("open")); }
        self._pump();
      } else if (d.op === "msg") {
        if (self.readyState === 1) self.dispatchEvent(new MessageEvent("message", {data: d.text, origin: location.origin}));
      } else if (d.op === "done") {
        self._busy = false;
        if (d.frame) measure(self, d.ms);
        self._pump();
      } else if (d.op === "dump") {
        warnOnce(kind + ": " + d.text);
      } else if (d.op === "status") {
        VV.status = d.text;
        changed();
      }
    };
    w.onerror = function (e) {
      warnOnce(kind + " worker: " + (e.message || e));
      if (self._worker === w && self.readyState < 2) self._fail();
    };
    w.postMessage({op: "start", silent: silent, replay: silent ? self._state.slice() : [], base: VV.recBase, id: self._id});
  };
  VVSocket.prototype._switch = function () {
    if (this.readyState > 1) return;
    var old = this._worker;
    this._worker = null;
    if (old) { old.postMessage({op: "close"}); setTimeout(function () { old.terminate(); }, 1000); }
    this._queue = this._queue.filter(function (t) { return !FRAME.test(t); });
    this._start(this.readyState === 1);
  };
  VVSocket.prototype._pump = function () {
    if (this._busy || !this._worker || !this._queue.length) return;
    var text = this._queue.shift();
    this._busy = true;
    this._worker.postMessage({op: "send", text: text, frame: FRAME.test(text)});
  };
  VVSocket.prototype.send = function (data) {
    if (this.readyState === 0) throw new DOMException("Still in CONNECTING state.", "InvalidStateError");
    if (this.readyState !== 1) return;
    var text = String(data);
    if (/"cmd":"(load_demo|set_mode)"/.test(text)) {
      var cmd = /"cmd":"(\w+)"/.exec(text)[1];
      this._state = this._state.filter(function (t) { return t.indexOf('"cmd":"' + cmd + '"') < 0; });
      this._state.push(text);
    }
    // One frame request in flight: a newer one replaces the one still waiting,
    // as a player asks by the music's clock and an old frame is worth nothing.
    var last = this._queue.length - 1;
    if (FRAME.test(text) && last >= 0 && FRAME.test(this._queue[last])) { this._queue[last] = text; VV.stats.dropped++; }
    else this._queue.push(text);
    this._pump();
  };
  VVSocket.prototype.close = function (code, reason) {
    if (this.readyState > 1) return;
    var self = this;
    self.readyState = 2;
    var w = self._worker;
    self._worker = null;
    if (w) { w.postMessage({op: "close"}); setTimeout(function () { w.terminate(); }, 1000); }
    VV.sockets = VV.sockets.filter(function (s) { return s !== self; });
    setTimeout(function () {
      self.readyState = 3;
      self.dispatchEvent(new CloseEvent("close", {code: code || 1000, reason: reason || "", wasClean: true}));
    }, 0);
  };
  VVSocket.prototype._fail = function () {
    this.dispatchEvent(new Event("error"));
    this.close(1006, "worker failed");
  };
  window.WebSocket = VVSocket;

  var warned = {};
  function warnOnce(text) {
    if (warned[text]) return;
    warned[text] = 1;
    console.warn("[vivid-vibes] " + text);
  }

  // ---- can the worker keep the frame rate? (auto mode only)
  // The player asks for a frame every interval = seconds per tick / frames per
  // tick (24.7 ms at 152 BPM). Two measurements decide:
  //  1. a probe: a separate live worker renders PROBE.count frames of the
  //     heaviest scene of the demo (julia_morph, 45 ms a frame in desktop
  //     Chromium); a median above PROBE_LIMIT ms, or no answer in PROBE_WAIT
  //     ms, means recorded;
  //  2. the first FIRST frames of the play itself: a median above the
  //     interval means recorded.
  var INTERVAL = 1000 * 1.5789473684210527 / 64;
  var PROBE = {demo: "main", tick: 104 * 64 + 16, count: 12};
  var PROBE_LIMIT = 2.5 * INTERVAL;
  var PROBE_WAIT = 4000;
  var FIRST = 64;
  var median = function (a) { var b = a.slice().sort(function (x, y) { return x - y; }); return b.length ? b[b.length >> 1] : 0; };

  function measure(sock, ms) {
    var st = VV.stats;
    st.frames++;
    st.ms.push(ms);
    if (st.ms.length > 120) st.ms.shift();
    if (VV.auto && VV.mode === "live" && st.frames === FIRST && median(st.ms) > INTERVAL) {
      VV.recReady.then(function (ok) { if (ok && VV.auto) setMode("recorded", "first " + FIRST + " frames: median " + median(st.ms).toFixed(1) + " ms > " + INTERVAL.toFixed(1) + " ms"); });
    }
    if (st.frames % 20 === 0) changed();
  }
  VV.median = median;
  VV.interval = INTERVAL;

  function probe() {
    var w = new Worker(new URL("vv/live-worker.js", here), {type: "module"});
    var done = false;
    var finish = function (ms, why) {
      if (done) return;
      done = true;
      w.terminate();
      VV.probe = {ms: ms, why: why};
      if (VV.auto && VV.mode === "live" && (ms === null || ms > PROBE_LIMIT)) {
        VV.recReady.then(function (ok) { if (ok && VV.auto) setMode("recorded", why); });
      }
      changed();
    };
    w.onmessage = function (e) {
      if (e.data.op !== "probe") return;
      var m = median(e.data.ms);
      finish(m, "probe: median " + m.toFixed(1) + " ms a frame of a heavy scene (limit " + PROBE_LIMIT.toFixed(0) + " ms)");
    };
    w.onerror = function () { finish(null, "probe failed"); };
    setTimeout(function () { finish(null, "probe: no answer in " + PROBE_WAIT + " ms"); }, PROBE_WAIT);
    w.postMessage({op: "probe", demo: PROBE.demo, tick: PROBE.tick, count: PROBE.count});
  }
  if (VV.auto) VV.recReady.then(function (ok) { if (ok) probe(); });

  // ---- the switch
  function ui() {
    var box = document.createElement("div");
    box.id = "vv-mode";
    box.style.cssText = "position:fixed;top:8px;right:8px;z-index:2000;font:12px monospace;color:#0f0;background:rgba(0,0,0,.8);" +
      "border:1px solid #0f0;padding:6px 8px;max-width:340px;line-height:1.4";
    var mk = function (mode, label) {
      var b = document.createElement("button");
      b.type = "button";
      b.textContent = label;
      b.dataset.mode = mode;
      b.style.cssText = "font:12px monospace;padding:3px 8px;margin:0 4px 0 0;border:1px solid #0f0;cursor:pointer";
      b.onclick = function () { VV.setMode(mode); };
      return b;
    };
    var live = mk("live", "LIVE"), rec = mk("recorded", "RECORDED");
    var info = document.createElement("div");
    info.style.cssText = "color:#8f8;margin-top:4px;white-space:normal";
    box.append(live, rec, info);
    document.body.appendChild(box);
    var paint = function () {
      [live, rec].forEach(function (b) {
        var on = b.dataset.mode === VV.mode;
        b.style.background = on ? "#0f0" : "#000";
        b.style.color = on ? "#000" : "#0f0";
      });
      rec.disabled = VV.recorded === false;
      rec.title = VV.recorded === false ? "the recorded frames are not published with this build" : "frames recorded from the Go build";
      rec.style.opacity = rec.disabled ? "0.4" : "1";
      var st = VV.stats, text;
      if (VV.mode === "live") text = "ABAP compiled to JS, running in this tab" + (st.frames ? " | " + median(st.ms).toFixed(1) + " ms a frame" : "");
      else text = "frames recorded from the Go build" + (VV.status ? " | " + VV.status : "");
      if (VV.auto && VV.why) text += " | auto: " + VV.why;
      info.textContent = text;
    };
    VV.listeners.push(paint);
    paint();
  }
  if (document.readyState === "loading") document.addEventListener("DOMContentLoaded", ui);
  else ui();
})();
