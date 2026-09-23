// The chunk format of the recorded mode (vv-rec-1), shared by the packer
// (tools/pages/pack.mjs, Node) and the replay worker (the browser).
//
// A chunk is gzip over UTF-8 text: a header line "vv1 <mode> <first> <count>",
// then one line per frame. Frame texts are JSON and never hold a raw newline,
// NUL or U+0001. An empty line is a frame the ABAP did not produce (a dump).
//   mode r: each line is the frame's text
//   mode d: each line is a token delta against the previous frame of the chunk:
//           parts joined by NUL, a part "\u0001<n>" copies the next n tokens
//           of the previous frame at the same positions, any other part is a
//           literal token. The first frame of a chunk has no previous one.
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
