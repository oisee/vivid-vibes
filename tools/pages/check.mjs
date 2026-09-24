// Checks a served build of pages/ in headless Chromium (Playwright):
//
//   node tools/pages/check.mjs --url http://127.0.0.1:3197/ --frames <work>/frames.ndjson
//        [--frames-pure <work>/frames-pure.ndjson] [--only live,recorded,equal,auto,switch,dev]
//
//   live, recorded  the megademo player plays several scenes (the audio is
//                   moved to each scene's first bar) with no console error,
//                   page error or failed request, and frames arrive for each
//   equal           frames of the live worker (JS in the browser) against the
//                   Go builds (libm, the recording: byte-equal, or the last
//                   bit of sin where V8 differs from glibc; pure: reported):
//                   ticks 0-2047
//                   in one run, then 64 frames from the start of three heavy
//                   scenes, each on a fresh socket
//   auto            the default page stays live on this machine and falls
//                   back to recorded when the probe's limit is lowered to 5 ms
//   switch          the LIVE / RECORDED buttons swap the worker under an open
//                   socket while the demo plays, and frames keep coming
//   dev             the dev player connects and PRELOAD ALL runs its sockets
//   norec           (a site built without rec/, not in the default list) live
//                   only, RECORDED disabled, no errors
//
// Playwright is resolved from the current directory (run it from a checkout
// that has it installed, e.g. open-steamgate).
import {createReadStream} from "node:fs";
import {tmpdir} from "node:os";
import {createRequire} from "node:module";
import {join} from "node:path";
import {createInterface} from "node:readline";

const {chromium} = createRequire(join(process.cwd(), "noop.js"))("playwright");
const arg = (k, d) => { const i = process.argv.indexOf(k); return i < 0 ? d : process.argv[i + 1]; };
const base = new URL(arg("--url", "http://127.0.0.1:3197/"));
const only = (arg("--only", "live,recorded,equal,auto,switch,dev")).split(",");
const BAR = 1.5789473684210527;
const SCENES = [[0, "Sales Dance"], [16, "plasma"], [32, "rotozoom"], [56, "amiga_ball"], [64, "sierpinski"], [76, "joydivision"], [100, "torus_3d"], [104, "julia_morph"]];
let failed = 0;
const ok = (cond, text) => { console.log(`${cond ? "ok  " : "FAIL"} ${text}`); if (!cond) failed++; };

const browser = await chromium.launch({args: ["--autoplay-policy=no-user-gesture-required"]});
async function open(path) {
  const page = await browser.newPage();
  const errors = [];
  page.on("console", (m) => { if (m.type() === "error") errors.push(`console: ${m.text()}`); });
  page.on("pageerror", (e) => errors.push(`pageerror: ${e.message}`));
  page.on("requestfailed", (r) => errors.push(`requestfailed: ${r.url()} ${r.failure()?.errorText}`));
  page.on("response", (r) => { if (r.status() >= 400) errors.push(`${r.status()} ${r.url()}`); });
  await page.goto(new URL(path, base).href);
  return {page, errors};
}

async function play(mode) {
  const {page, errors} = await open(`index.html?mode=${mode}`);
  await page.waitForFunction(() => /Ready!/.test(document.getElementById("info").textContent), null, {timeout: 30000});
  await page.evaluate(() => {
    window.__n = {};
    VV.sockets[0].addEventListener("message", (e) => { const m = /"e":"([^"]*)"/.exec(e.data); if (m) window.__n[m[1]] = (window.__n[m[1]] || 0) + 1; });
  });
  await page.click("#btn-start");
  await page.waitForFunction(() => /►/.test(document.getElementById("part-info").textContent), null, {timeout: 30000});
  for (const [bar, effect] of SCENES) {
    await page.evaluate((t) => { document.getElementById("au").currentTime = t; }, bar * BAR + 0.3);
    await page.waitForTimeout(2500);
    const n = await page.evaluate((e) => window.__n[e] || 0, effect);
    ok(n >= 20, `${mode}: ${effect} (bar ${bar}): ${n} frames in 2.5 s`);
  }
  if (mode === "recorded") {
    // decoding a chunk (fetch not counted) must stay well under its play time
    const cs = await page.evaluate(() => VV.chunks || []);
    const ms = cs.map((c) => c.ms).sort((a, b) => a - b);
    const top = cs.reduce((a, c) => (c.ms > a.ms ? c : a), {ms: 0});
    console.log(`     ${cs.length} chunks decoded, median ${ms[ms.length >> 1]?.toFixed(0)} ms, slowest ${top.file} ${top.ms.toFixed(0)} ms (${(top.bytes / 1e6).toFixed(2)} MB)`);
    ok(cs.length > 0 && top.ms < BAR * 1000 / 2, `recorded: every chunk decoded in under half its play time (${top.ms.toFixed(0)} ms < ${(BAR * 500).toFixed(0)} ms)`);
  }
  await page.screenshot({path: join(arg("--shots", tmpdir()), `vv-check-${mode}.png`)});
  ok(await page.evaluate(() => VV.mode) === mode, `${mode}: still ${mode} at the end`);
  ok(errors.length === 0, `${mode}: no console errors, page errors or failed requests${errors.length ? `: ${errors.slice(0, 5).join(" | ")}` : ""}`);
  await page.close();
}

async function expected(want, file) {
  const got = new Map();
  const rl = createInterface({input: createReadStream(file), crlfDelay: Infinity});
  for await (const l of rl) {
    if (!l.startsWith('{"part":"main"')) continue;
    const i = Number(/"i":(-?\d+)/.exec(l)[1]);
    if (!want.has(i)) continue;
    const r = JSON.parse(l);
    got.set(i, r.text ?? null);
  }
  return got;
}

// the largest relative difference of two frames' numbers, or "structure"
function worst(a, b) {
  let w = 0, struct = false;
  const walk = (p, q) => {
    if (Array.isArray(p)) { if (!Array.isArray(q) || p.length !== q.length) { struct = true; return; } p.forEach((v, k) => walk(v, q[k])); return; }
    if (p && typeof p === "object") { for (const k of Object.keys(p)) walk(p[k], q?.[k]); return; }
    if (typeof p === "number" && typeof q === "number") { if (p !== q) w = Math.max(w, Math.abs(p - q) / Math.max(Math.abs(p), Math.abs(q))); }
    else if (p !== q) struct = true;
  };
  walk(JSON.parse(a), JSON.parse(b));
  return struct ? "structure" : w;
}

async function equal() {
  const runs = [{from: 0, n: 2048}, {from: 56 * 64, n: 64}, {from: 64 * 64, n: 64}, {from: 104 * 64, n: 64}];
  const want = new Set(runs.flatMap((r) => Array.from({length: r.n}, (_, k) => r.from + k)));
  // the Go builds: libm (glibc sin/cos, equal to A4H, what the recorded mode
  // plays) and, when given, pure (gogen's default fdlibm port)
  const builds = [["Go libm", arg("--frames")], ["pure Go", arg("--frames-pure")]].filter(([, f]) => f);
  const exp = await Promise.all(builds.map(([, f]) => expected(want, f)));
  const {page, errors} = await open("index.html?mode=live");
  for (const r of runs) {
    // a fresh socket (a fresh worker, fresh class data) per run, frames asked one by one
    const texts = await page.evaluate(async ({from, n}) => {
      const ws = new WebSocket(`ws://${location.host}/sap/bc/apc/sap/zo4d_demo`);
      const inbox = [];
      let wake = null;
      ws.onmessage = (e) => { inbox.push(e.data); if (wake) wake(); };
      await new Promise((ok) => { ws.onopen = ok; });
      const next = async () => { while (!inbox.length) await new Promise((ok) => { wake = ok; }); return inbox.shift(); };
      await next(); // the config ON_START sends
      const out = [];
      for (let i = from; i < from + n; i++) {
        ws.send(JSON.stringify({cmd: "frame", tick: i, sub: 0}));
        out.push(await next());
      }
      ws.close();
      return out;
    }, r);
    builds.forEach(([name], b) => {
      let same = 0, bad = 0;
      const differ = new Map();
      texts.forEach((t, k) => {
        const i = r.from + k, ref = exp[b].get(i);
        if (t === ref) { same++; return; }
        const e = /"e":"([^"]*)"/.exec(t)?.[1];
        const w = ref == null ? "missing" : worst(t, ref);
        const d = differ.get(e) ?? {n: 0, rel: 0, struct: 0};
        d.n++;
        if (typeof w === "number") d.rel = Math.max(d.rel, w); else d.struct++;
        differ.set(e, d);
        // the last bit of sin / cos (V8 is not glibc) is expected; a larger
        // difference only where particles are seeded from sin chains (ignition)
        if (!(typeof w === "number" && w < 1e-14) && !/^ignit/.test(e)) bad++;
      });
      const note = differ.size ? `; differ: ${[...differ].map(([e, d]) => `${e} ${d.n}${d.struct ? ` (${d.struct} in structure)` : ` (rel. <= ${d.rel.toExponential(1)})`}`).join(", ")}` : "";
      ok(name !== "Go libm" || bad === 0, `equal: ticks ${r.from}-${r.from + r.n - 1}: ${same}/${r.n} live frames byte-equal to ${name}${note}`);
    });
  }
  ok(errors.length === 0, `equal: no console errors${errors.length ? `: ${errors.slice(0, 3).join(" | ")}` : ""}`);
  await page.close();
}

async function auto() {
  {
    const {page, errors} = await open("index.html");
    await page.waitForFunction(() => VV.probe, null, {timeout: 20000});
    const r = await page.evaluate(() => ({mode: VV.mode, probe: VV.probe}));
    ok(r.mode === "live", `auto: this machine stays live (${r.probe.why})`);
    ok(errors.length === 0, "auto: no console errors");
    await page.close();
  }
  {
    const page = await browser.newPage();
    // CPU throttling in DevTools slows the page, not its workers: a device
    // too slow for the probe is played by lowering the probe's limit instead
    await page.goto(new URL("index.html?probe-limit=5", base).href);
    await page.waitForFunction(() => VV.mode === "recorded", null, {timeout: 60000}).catch(() => {});
    const r = await page.evaluate(() => ({mode: VV.mode, why: VV.why}));
    ok(r.mode === "recorded", `auto: a device over the probe's limit falls back to recorded (${r.why})`);
    await page.close();
  }
}

async function toggle() {
  const {page, errors} = await open("index.html");
  await page.waitForFunction(() => /Ready!/.test(document.getElementById("info").textContent), null, {timeout: 30000});
  await page.evaluate(() => { window.__f = 0; VV.sockets[0].addEventListener("message", (e) => { if (/"e":"/.test(e.data)) window.__f++; }); });
  await page.click("#btn-start");
  await page.waitForFunction(() => /►/.test(document.getElementById("part-info").textContent), null, {timeout: 30000});
  await page.evaluate((t) => { document.getElementById("au").currentTime = t; }, 16 * BAR);
  for (const [button, mode] of [["RECORDED", "recorded"], ["LIVE", "live"], ["RECORDED", "recorded"]]) {
    await page.click(`#vv-mode button[data-mode=${mode}]`);
    const before = await page.evaluate(() => window.__f);
    await page.waitForTimeout(2500);
    const r = await page.evaluate(() => ({f: window.__f, mode: VV.mode, kind: VV.sockets[0]._kind, open: VV.sockets[0].readyState}));
    ok(r.mode === mode && r.kind === (mode === "live" ? "live" : "replay") && r.open === 1 && r.f - before >= 20,
      `switch: ${button} while playing: ${r.f - before} frames in 2.5 s from the ${r.kind} worker, the socket still open`);
  }
  ok(errors.length === 0, `switch: no console errors${errors.length ? `: ${errors.slice(0, 3).join(" | ")}` : ""}`);
  await page.close();
}

async function norec() {
  const {page, errors} = await open("index.html");
  await page.waitForFunction(() => VV.recorded === false, null, {timeout: 20000});
  const r = await page.evaluate(() => ({mode: VV.mode, disabled: document.querySelector("#vv-mode button[data-mode=recorded]").disabled}));
  ok(r.mode === "live" && r.disabled, "norec: without published frames the page is live and RECORDED is disabled");
  ok(errors.length === 0, `norec: no console errors${errors.length ? `: ${errors.slice(0, 3).join(" | ")}` : ""}`);
  await page.close();
}

async function dev() {
  const {page, errors} = await open("index.html?player=dev&demo=main&mode=live");
  await page.waitForURL(/dev-main\.html/);
  await page.waitForTimeout(3000);
  await page.click("#b-preload");
  await page.waitForTimeout(6000);
  const r = await page.evaluate(() => ({sockets: VV.sockets.length}));
  ok(r.sockets > 1, `dev: PRELOAD ALL opened ${r.sockets} sockets, one worker each`);
  ok(errors.length === 0, `dev: no console errors${errors.length ? `: ${errors.slice(0, 3).join(" | ")}` : ""}`);
  await page.close();
}

if (only.includes("live")) await play("live");
if (only.includes("recorded")) await play("recorded");
if (only.includes("equal")) await equal();
if (only.includes("auto")) await auto();
if (only.includes("switch")) await toggle();
if (only.includes("dev")) await dev();
if (only.includes("norec")) await norec();
await browser.close();
console.log(failed ? `${failed} failed` : "all passed");
process.exit(failed ? 1 : 0);
