// The recorded mode: one APC connection answered from frames recorded from
// the Go build of the same ABAP (sin and cos from glibc, the build that
// equals A4H frame for frame). The page asks for a frame by the music's
// clock ({"cmd":"frame","tick","sub"}), so answering by tick keeps the
// picture in step with the music. Frames come in chunks of one bar
// (rec-codec.js), fetched when first needed and two ahead; a frame whose
// chunk is still on its way is not answered, as a slow server would not.
import {decodeChunk} from "./rec-codec.js";

let base = "";
let index = null;
let demo = "main";
let mode = "viewer";
const chunks = new Map(); // file -> {frames} | Promise
let last = 0;

const send = (text) => postMessage({op: "msg", text});
const part = () => index.parts[demo] ?? index.parts.main;
const typed = (list, type) => list.find((t) => t.startsWith(`{"type":"${type}"`));
const num = (text, key) => { const m = new RegExp(`"${key}":(\\d+(?:\\.\\d+)?)`).exec(text); return m ? Number(m[1]) : 0; };

function chunkAt(i) {
  const cs = part().chunks;
  let lo = 0, hi = cs.length - 1;
  while (lo <= hi) {
    const mid = (lo + hi) >> 1;
    if (i < cs[mid].first) hi = mid - 1;
    else if (i >= cs[mid].first + cs[mid].n) lo = mid + 1;
    else return {c: cs[mid], k: mid};
  }
  return null;
}
function load(c) {
  let got = chunks.get(c.file);
  if (got) return got instanceof Promise ? got : Promise.resolve(got);
  got = fetch(base + c.file).then((r) => {
    if (!r.ok) throw new Error(`${c.file}: ${r.status}`);
    return r.arrayBuffer();
  }).then((buf) => {
    // gzip, unless a server already undid it with a Content-Encoding
    const b = new Uint8Array(buf);
    if (b[0] !== 0x1f || b[1] !== 0x8b) return new TextDecoder().decode(b);
    return new Response(new Blob([b]).stream().pipeThrough(new DecompressionStream("gzip"))).text();
  }).then((text) => {
    const v = decodeChunk(text);
    chunks.set(c.file, v);
    return v;
  }, (e) => { chunks.delete(c.file); postMessage({op: "dump", text: String(e.message ?? e)}); throw e; });
  chunks.set(c.file, got);
  return got;
}
function frameAt(i, wait) {
  const at = chunkAt(i);
  if (!at) return null;
  const cs = part().chunks;
  for (let j = 1; j <= 2 && at.k + j < cs.length; j++) load(cs[at.k + j]).catch(() => {});
  // keep the chunk played, the one before and what is ahead
  if (chunks.size > 6) for (const [file] of chunks) if (!cs.slice(Math.max(0, at.k - 1), at.k + 3).some((c) => c.file === file)) chunks.delete(file);
  const got = chunks.get(at.c.file);
  if (got && !(got instanceof Promise)) {
    if (at.k !== last) { last = at.k; postMessage({op: "status", text: `${at.c.scene}, bar ${Math.floor(i / 64 / part().fpt)}`}); }
    return got.frames[i - got.first];
  }
  const p = load(at.c);
  return wait ? p.then((v) => v.frames[i - v.first]) : null;
}

async function handle(text) {
  if (!text.startsWith("{")) {
    if (text === "scenario") send(typed(part().control, "scenario"));
    return;
  }
  const cmd = /"cmd":"(\w+)"/.exec(text)?.[1];
  switch (cmd) {
    case "get_megademo": send(typed(index.start, "megademo")); break;
    case "get_scenario": send(typed(part().control, "scenario")); break;
    case "load_demo": {
      const id = /"demo":"([^"]*)"/.exec(text)?.[1];
      if (!id) break;
      demo = index.parts[id] ? id : "main";
      send(typed(part().control, "config"));
      break;
    }
    case "set_mode": {
      const m = /"mode":"([^"]*)"/.exec(text)?.[1];
      if (m === "dev" || m === "viewer") { mode = m; send(`{"type":"mode","mode":"${mode}"}`); }
      break;
    }
    case "frame": {
      const i = num(text, "tick") * part().fpt + num(text, "sub");
      const f = frameAt(i, false);
      if (f) send(f);
      break;
    }
    case "preload": {
      // the dev player's PREFETCH: the recorded frame, labelled as preload
      const n = num(text, "frame");
      const f = await frameAt(n, true);
      if (f) send(`{"type":"preload","pf":${n},` + f.slice(1));
      break;
    }
    case "seek": {
      const cfg = JSON.parse(typed(part().control, "config"));
      const f = text.includes('"bar":') ? Math.floor(num(text, "bar") * cfg.bar_sec * cfg.fps) : num(text, "frame");
      send(`{"type":"seeked","frame":${f}}`);
      break;
    }
    case "flash": send(`{"type":"flash_ack","intensity":${num(text, "intensity") || 1}}`); break;
  }
}

let ready = null;
onmessage = async (e) => {
  const d = e.data;
  if (d.op === "start") {
    base = d.base;
    ready = fetch(base + "index.json").then((r) => r.json()).then((j) => { index = j; });
    try { await ready; } catch (x) { postMessage({op: "dump", text: `recorded frames: ${x.message ?? x}`}); throw x; }
    for (const t of d.replay ?? []) {
      const id = /"cmd":"load_demo".*"demo":"([^"]*)"/.exec(t)?.[1];
      if (id) demo = index.parts[id] ? id : "main";
    }
    // warm the first chunk of the part, so the first frames are there
    if (part().chunks[0]) load(part().chunks[0]).catch(() => {});
    postMessage({op: "open"});
    if (!d.silent) for (const t of index.start) if (!t.startsWith('{"type":"megademo"')) send(t);
  } else if (d.op === "send") {
    await ready;
    const t0 = performance.now();
    try { await handle(d.text); } catch (x) { postMessage({op: "dump", text: String(x.message ?? x)}); }
    postMessage({op: "done", frame: d.frame, ms: performance.now() - t0});
  }
};
