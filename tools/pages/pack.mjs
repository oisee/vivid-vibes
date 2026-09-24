// Packs the recording of tools/pages/build.mjs (record) into chunks of the
// vv-rec-2 format (pages/vv/rec-codec.js): per part, per scene of the part's
// scenario, at most CHUNK frames each, every chunk standing alone so the
// replay can load any of them by itself. Each chunk is compressed with zstd
// (needs Node >= 22.15), then decompressed and decoded again and compared
// with the recording, frame by frame and byte for byte, before it counts.
import {createReadStream, mkdirSync, writeFileSync} from "node:fs";
import {join} from "node:path";
import {createInterface} from "node:readline";
import {constants as Z, zstdCompressSync, zstdDecompressSync} from "node:zlib";
import {decodeChunk2, encodeChunk2} from "../../pages/vv/rec-codec.js";

// level 19 with a 128 MB long-distance window: a chunk of the heaviest scene
// is 60 MB of text, and its frames repeat each other from a frame (500 KB) back
const ZSTD = {params: {[Z.ZSTD_c_compressionLevel]: 19, [Z.ZSTD_c_windowLog]: 27, [Z.ZSTD_c_enableLongDistanceMatching]: 1, [Z.ZSTD_c_checksumFlag]: 1}};

const CHUNK = 64; // one bar at one frame per tick

export async function pack(file, dir) {
  mkdirSync(dir, {recursive: true});
  const index = {format: "vv-rec-2", start: [], parts: {}};
  const rl = createInterface({input: createReadStream(file), crlfDelay: Infinity});
  let part = null; // {id, scenes, fpt, chunks, frames, first, raw, bytes, dumps}
  const flush = () => {
    if (!part || part.frames.length === 0) return;
    const {frames, first} = part;
    const text = Buffer.from(encodeChunk2(frames, first));
    const zst = zstdCompressSync(text, ZSTD);
    const back = decodeChunk2(new Uint8Array(zstdDecompressSync(zst)));
    if (back.first !== first || back.n !== frames.length) throw new Error(`chunk ${part.id}/${first}: wrong header`);
    for (let k = 0; k < frames.length; k++) if (back.frame(k) !== (frames[k] ?? null)) throw new Error(`chunk ${part.id}/${first}: frame ${first + k} does not decode to the recording`);
    const raw = frames.reduce((a, f) => a + (f == null ? 1 : Buffer.byteLength(f) + 1), 0);
    const name = `${part.id}/${String(first).padStart(5, "0")}.zst`;
    mkdirSync(join(dir, part.id), {recursive: true});
    writeFileSync(join(dir, name), zst);
    part.chunks.push({first, n: frames.length, file: name, bytes: zst.length, scene: part.scene});
    part.raw += raw;
    part.bytes += zst.length;
    part.checked += frames.length;
    part.frames = [];
  };
  const close = () => {
    if (!part) return;
    flush();
    const p = index.parts[part.id] = {control: part.control, fpt: part.fpt, frames: part.n, dumps: part.dumps, raw: part.raw, bytes: part.bytes, chunks: part.chunks};
    console.log(`${part.id}: ${p.frames} frames (${p.dumps} dumped), ${(p.raw / 1e6).toFixed(1)} MB -> ${(p.bytes / 1e6).toFixed(2)} MB zstd in ${p.chunks.length} chunks, ${part.checked} frames checked`);
  };
  for await (const l of rl) {
    const r = JSON.parse(l);
    if (!r.part) { index.start.push(r.text); continue; }
    if (!part || part.id !== r.part) { close(); part = {id: r.part, control: [], chunks: [], frames: [], first: 0, n: 0, raw: 0, bytes: 0, dumps: 0, fpt: 1, checked: 0}; }
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
  const all = Object.values(index.parts).flatMap((p) => p.chunks);
  const big = all.reduce((a, c) => (c.bytes > a.bytes ? c : a), {bytes: 0});
  console.log(`${dir}: ${(total / 1e6).toFixed(2)} MB in ${all.length} chunks, the largest ${big.file} ${(big.bytes / 1e6).toFixed(2)} MB`);
  return {index, total};
}
