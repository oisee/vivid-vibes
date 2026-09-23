// Packs the recording of tools/pages/build.mjs (record) into chunks of the
// vv-rec-1 format (pages/vv/rec-codec.js): per part, per scene of the part's
// scenario, at most CHUNK frames each, every chunk starting with a whole
// frame so the replay can load any of them alone. Each chunk is written in
// the smaller of the two modes (text or token delta) after gzip, and is
// decoded again and compared with the recording before it counts.
import {createReadStream, mkdirSync, writeFileSync} from "node:fs";
import {join} from "node:path";
import {createInterface} from "node:readline";
import {gzipSync} from "node:zlib";
import {decodeChunk, encodeDelta} from "../../pages/vv/rec-codec.js";

const CHUNK = 64; // one bar at one frame per tick

export async function pack(file, dir) {
  mkdirSync(dir, {recursive: true});
  const index = {format: "vv-rec-1", start: [], parts: {}};
  const rl = createInterface({input: createReadStream(file), crlfDelay: Infinity});
  let part = null; // {id, scenes, fpt, chunks, frames, first, raw, bytes, dumps}
  const flush = () => {
    if (!part || part.frames.length === 0) return;
    const {frames, first} = part;
    for (const f of frames) if (f != null && /[\n\u0000\u0001]/.test(f)) throw new Error(`frame ${first}: a raw newline, NUL or U+0001`);
    const head = (mode) => `vv1 ${mode} ${first} ${frames.length}\n`;
    const raw = Buffer.from(head("r") + frames.map((f) => f ?? "").join("\n"));
    const delta = Buffer.from(head("d") + encodeDelta(frames).join("\n"));
    const gr = gzipSync(raw, {level: 9});
    const gd = gzipSync(delta, {level: 9});
    const [mode, gz, text] = gd.length < gr.length ? ["d", gd, delta] : ["r", gr, raw];
    const back = decodeChunk(text.toString("utf8"));
    if (back.first !== first || back.frames.length !== frames.length || back.frames.some((f, k) => f !== (frames[k] ?? null))) throw new Error(`chunk ${part.id}/${first} does not decode to the recording`);
    const name = `${part.id}/${String(first).padStart(5, "0")}.gz`;
    mkdirSync(join(dir, part.id), {recursive: true});
    writeFileSync(join(dir, name), gz);
    part.chunks.push({first, n: frames.length, file: name, bytes: gz.length, mode, scene: part.scene});
    part.raw += raw.length;
    part.bytes += gz.length;
    part.frames = [];
  };
  const close = () => {
    if (!part) return;
    flush();
    const p = index.parts[part.id] = {control: part.control, fpt: part.fpt, frames: part.n, dumps: part.dumps, raw: part.raw, bytes: part.bytes, chunks: part.chunks};
    console.log(`${part.id}: ${p.frames} frames (${p.dumps} dumped), ${(p.raw / 1e6).toFixed(1)} MB -> ${(p.bytes / 1e6).toFixed(2)} MB gzip in ${p.chunks.length} chunks`);
  };
  for await (const l of rl) {
    const r = JSON.parse(l);
    if (!r.part) { index.start.push(r.text); continue; }
    if (!part || part.id !== r.part) { close(); part = {id: r.part, control: [], chunks: [], frames: [], first: 0, n: 0, raw: 0, bytes: 0, dumps: 0, fpt: 1}; }
    if (r.i < 0) {
      if (r.text == null) throw new Error(`${r.part}: a control step dumped: ${r.dump}`);
      part.control.push(r.text);
      const j = JSON.parse(r.text);
      if (j.type === "config") part.fpt = j.fpt;
      if (j.type === "scenario") part.scenes = j.scenes;
      continue;
    }
    if (r.i !== part.n) { if (r.i === part.n - 1 && r.dump) { part.dumps++; part.frames[part.frames.length - 1] = null; continue; } throw new Error(`${r.part}: frame ${r.i} out of order`); }
    const bar = Math.floor(r.i / (64 * part.fpt));
    const scene = part.scenes?.find((s) => bar >= s.start_bar && bar < s.end_bar)?.id ?? "";
    if (part.frames.length === CHUNK * part.fpt || (part.frames.length && scene !== part.scene)) flush();
    if (part.frames.length === 0) { part.first = r.i; part.scene = scene; }
    part.frames.push(r.dump ? null : r.text);
    if (r.dump) part.dumps++;
    part.n++;
  }
  close();
  writeFileSync(join(dir, "index.json"), JSON.stringify(index) + "\n");
  const total = Object.values(index.parts).reduce((a, p) => a + p.bytes, 0);
  console.log(`${dir}: ${(total / 1e6).toFixed(2)} MB in ${Object.values(index.parts).reduce((a, p) => a + p.chunks.length, 0)} chunks`);
  return {index, total};
}
