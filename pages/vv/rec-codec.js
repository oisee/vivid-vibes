// The chunk formats of the recorded mode, shared by the packer
// (tools/pages/pack.mjs, Node) and the replay worker (the browser).
// Frame texts are JSON and never hold a raw newline, NUL or U+0001.
//
// vv-rec-2 (written now): zstd (level 19, 128 MB long-distance window) over
// the text below, decoded in the browser by the vendored fzstd (fzstd.js).
//
//   "vv2 <first> <count>\n"
//   <count> skeleton lines: the frame's text with every number replaced by
//     U+0001; an empty line is a frame the ABAP did not produce (a dump)
//   one line per column: the numbers of one context, joined by ","
//
// A number is a match of NUM; its context is the last (up to) six bytes of
// UTF-8 before it since the previous number or the start of the frame (`"x":`,
// `,"y":`, `hsl(`). The columns come in the order their context is first
// met, reading the skeleton lines in order, so the decoder assigns them
// without a table. Numbers go back as the digits they were written with,
// which is why the result is byte-identical: nothing is parsed as a float.
// Taking the numbers out of the text is the whole trick: the skeleton of a
// frame of 5000 circles repeats itself and costs almost nothing, and each
// column holds numbers of one kind, which the entropy coder likes. The
// 17-digit floats that change every frame are the floor either way.
//
// vv-rec-1 (still read): gzip over a header line "vv1 <mode> <first> <count>",
// then one line per frame.
//   mode r: each line is the frame's text
//   mode d: each line is a token delta against the previous frame of the chunk:
//           parts joined by NUL, a part "\u0001<n>" copies the next n tokens
//           of the previous frame at the same positions, any other part is a
//           literal token. The first frame of a chunk has no previous one.
export const NUM = /-?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?/g;

const utf8 = new TextEncoder();
// the context of a number: the last six bytes of UTF-8 of the literal before it
function context(lit) {
  if (!/[^\x00-\x7f]/.test(lit)) return lit.slice(-6);
  const b = utf8.encode(lit);
  return String.fromCharCode(...b.subarray(Math.max(0, b.length - 6)));
}

export function encodeChunk2(frames, first) {
  const skeleton = [], cols = new Map(), order = [];
  for (const f of frames) {
    if (f == null) { skeleton.push(""); continue; }
    if (f === "" || /[\n\u0000\u0001]/.test(f)) throw new Error(`frame ${first}: empty, or a raw newline, NUL or U+0001`);
    const parts = [];
    let last = 0;
    for (const m of f.matchAll(NUM)) {
      const lit = f.slice(last, m.index);
      parts.push(lit);
      last = m.index + m[0].length;
      const key = context(lit);
      let col = cols.get(key);
      if (!col) { col = []; cols.set(key, col); order.push(key); }
      col.push(m[0]);
    }
    parts.push(f.slice(last));
    skeleton.push(parts.join("\u0001"));
  }
  return `vv2 ${first} ${frames.length}\n` + skeleton.join("\n") + "\n" + order.map((k) => cols.get(k).join(",")).join("\n");
}

// bytes: the decompressed chunk. Returns {first, n, frame(k)}: the frames are
// rebuilt as UTF-8 into one buffer at once (the columns are read in order),
// a frame becomes a string only when it is asked for.
export function decodeChunk2(bytes) {
  let p = bytes.indexOf(10);
  const [magic, first, count] = new TextDecoder().decode(bytes.subarray(0, p)).split(" ");
  if (magic !== "vv2") throw new Error("not a vv-rec-2 chunk");
  const n = Number(count);
  // the columns start after the n-th skeleton line
  let q = p + 1;
  for (let k = 0; k < n; k++) { q = bytes.indexOf(10, q); if (q < 0) throw new Error("vv2: short skeleton"); q++; }
  let colNext = q; // where the next column not yet met starts
  // context (six bytes packed with their count into a number) -> the read
  // position in its column, open addressing; a Map was half the time
  const SIZE = 1 << 16;
  const keys = new Float64Array(SIZE).fill(-1), pos = new Int32Array(SIZE);
  let used = 0;
  const out = new Uint8Array(bytes.length);
  const starts = new Int32Array(n), ends = new Int32Array(n);
  let o = 0, i = p + 1;
  let slot = bytes.indexOf(1, i); // the next U+0001, -1 when there is none
  for (let k = 0; k < n; k++) {
    starts[k] = o;
    const end = bytes.indexOf(10, i);
    if (end === i) { ends[k] = -1; i++; continue; }
    for (;;) {
      if (slot >= 0 && slot < i) slot = bytes.indexOf(1, i);
      const s = slot >= 0 && slot < end ? slot : end;
      // the literal up to the number (or the end of the line) as it is
      if (s - i > 64) { out.set(bytes.subarray(i, s), o); o += s - i; }
      else for (let j = i; j < s; j++) out[o++] = bytes[j];
      if (s === end) break;
      // the context: up to six bytes before, packed with their count
      const from = Math.max(i, s - 6);
      let key = s - from, h = key;
      for (let j = from; j < s; j++) { key = key * 256 + bytes[j]; h = (h * 31 + bytes[j]) & (SIZE - 1); }
      while (keys[h] !== key && keys[h] !== -1) h = (h + 1) & (SIZE - 1);
      let at;
      if (keys[h] === key) at = pos[h];
      else {
        if (++used > SIZE / 2) throw new Error("vv2: too many contexts");
        keys[h] = key;
        at = colNext;
        const e = bytes.indexOf(10, colNext);
        colNext = e < 0 ? bytes.length : e + 1;
      }
      let c;
      while (at < bytes.length && (c = bytes[at]) !== 44 && c !== 10) { out[o++] = c; at++; }
      pos[h] = at + 1;
      i = s + 1;
    }
    ends[k] = o;
    i = end + 1;
  }
  const text = new TextDecoder();
  return {
    first: Number(first),
    n,
    frame: (k) => (k < 0 || k >= n || ends[k] < 0 ? null : text.decode(out.subarray(starts[k], ends[k]))),
  };
}

export const tokens = (s) => s.match(/-?\d+(?:\.\d+)?(?:[eE][-+]?\d+)?|[^-\d]+|-/g) || [];

export function encodeDelta(frames) {
  const out = [];
  let prev = [];
  for (const f of frames) {
    if (f == null) { out.push(""); continue; }
    const t = tokens(f);
    const parts = [];
    let run = 0;
    for (let i = 0; i < t.length; i++) {
      if (i < prev.length && prev[i] === t[i]) { run++; continue; }
      if (run) { parts.push("\u0001" + run); run = 0; }
      parts.push(t[i]);
    }
    if (run) parts.push("\u0001" + run);
    out.push(parts.join("\u0000"));
    prev = t;
  }
  return out;
}

export function decodeChunk(text) {
  const lines = text.split("\n");
  const [magic, mode, first, count] = lines[0].split(" ");
  if (magic !== "vv1") throw new Error("not a vv-rec-1 chunk");
  const n = Number(count);
  const frames = new Array(n);
  let prev = [];
  for (let k = 0; k < n; k++) {
    const line = lines[k + 1];
    if (line === "") { frames[k] = null; continue; }
    if (mode === "r") { frames[k] = line; continue; }
    const t = [];
    for (const p of line.split("\u0000")) {
      if (p.charCodeAt(0) === 1) { const run = Number(p.slice(1)); for (let j = 0; j < run; j++) t.push(prev[t.length]); }
      else t.push(p);
    }
    frames[k] = t.join("");
    prev = t;
  }
  return {first: Number(first), frames};
}
