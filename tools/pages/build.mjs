// Builds pages/ (the GitHub Pages site of the demo) out of this repository's
// ABAP with gogen, the experimental ABAP -> IR -> Go / JS compiler of
// open-steamgate. See tools/pages/README.md for the exact commits.
//
//   node tools/pages/build.mjs --osg <open-steamgate> --core <open-abap-core>/src --apc <open-abap-apc>/src
//        [--work <dir>] [--only live|record|pack]
//
//   live    the demo's ABAP as plain JS (pages/live/), the players (pages/*.html)
//   record  the Go build with sin/cos from glibc (-tags=libm, the build that
//           equals A4H) plays every frame of every part into <work>/frames.ndjson
//   pack    <work>/frames.ndjson -> --rec-out (default <work>/rec): chunks per scene,
//           token delta or text, gzip; see pages/vv/rec-codec.js
//
// Needs Node >= 22 and, for record, Go >= 1.26 with cgo and a C compiler.
import {execFileSync, spawn} from "node:child_process";
import {cpSync, createWriteStream, existsSync, mkdirSync, readFileSync, readdirSync, rmSync, writeFileSync} from "node:fs";
import {dirname, join, resolve} from "node:path";
import {fileURLToPath, pathToFileURL} from "node:url";
import {pack} from "./pack.mjs";

const here = dirname(fileURLToPath(import.meta.url));
const repo = resolve(here, "..", "..");
const arg = (k, d) => { const i = process.argv.indexOf(k); return i < 0 ? d : process.argv[i + 1]; };
const need = (k) => { const v = arg(k); if (!v) { console.error(`missing ${k} (see tools/pages/README.md)`); process.exit(2); } return resolve(v); };
const only = arg("--only");
const work = resolve(arg("--work", join(repo, ".pages-work")));
const pages = join(repo, "pages");
mkdirSync(work, {recursive: true});

// The effects the demo does not use and that gogen does not compile, and the
// two programs that need a GUI (the same list as open-steamgate's o4d pack).
const SKIP = /^(zcl_o4d_composer|zcl_o4d_mountains_oops_a)$/;

let program;
function compile() {
  if (program) return program;
  const osg = need("--osg");
  const gogen = join(osg, "tools", "gogen");
  return import(pathToFileURL(join(gogen, "frontend.mjs")).href).then(({compileProgram}) => {
    const src = join(repo, "src");
    const objects = ["zif_o4d_effect", ...new Set(readdirSync(src).filter((f) => /^zcl_o4d_.*\.clas\.abap$/.test(f))
      .map((f) => f.split(".")[0]).filter((n) => !SKIP.test(n)))].sort();
    program = compileProgram({folders: [src, need("--core"), need("--apc")], objects});
    console.log(`compiled ${program.classes.length} classes`);
    return program;
  });
}
const osgCommit = () => execFileSync("git", ["-C", need("--osg"), "rev-parse", "HEAD"]).toString().trim();

/* ------------------------------------------------------------------ live */
async function live() {
  const gogen = join(need("--osg"), "tools", "gogen");
  const {emitJs} = await import(pathToFileURL(join(gogen, "emit-js.mjs")).href);
  const p = await compile();
  const dir = join(pages, "live");
  mkdirSync(dir, {recursive: true});
  writeFileSync(join(dir, "demo.mjs"), emitJs(p, "./abap.mjs"));
  cpSync(join(gogen, "js", "abap.mjs"), join(dir, "abap.mjs"));
  // the players are what ZCL_O4D_HTTP_HANDLER serves, computed by the same JS
  const m = await import(pathToFileURL(join(dir, "demo.mjs")).href + `?t=${Date.now()}`);
  const s = {sy: {index: 0, tabix: 0, subrc: 0}};
  const hh = m.ZCL_O4D_HTTP_HANDLER.$new(s);
  const shim = `<script src="vv/shim.js"></script>`;
  // the one change to a player: the shim goes first, before any of its scripts
  const inject = (html) => { const at = html.indexOf("<head>") + 6; if (at < 6) throw new Error("no <head>"); return html.slice(0, at) + shim + html.slice(at); };
  writeFileSync(join(pages, "index.html"), inject(hh.GET_MEGADEMO_HTML(s)));
  for (const id of ["main", "outro"]) writeFileSync(join(pages, `dev-${id}.html`), inject(hh.GET_DEV_HTML(s, id)));
  writeFileSync(join(pages, "BUILD.json"), JSON.stringify({
    generator: "tools/pages/build.mjs", osg: osgCommit(), gogen: "tools/gogen (branch spike/go-backend)",
    classes: p.classes.length, source: execFileSync("git", ["-C", repo, "rev-parse", "HEAD"]).toString().trim(),
  }, null, 2) + "\n");
  console.log(`wrote pages/live/demo.mjs, pages/index.html, pages/dev-main.html, pages/dev-outro.html`);
}

/* ---------------------------------------------------------------- record */
async function record() {
  const osg = need("--osg");
  const gogen = join(osg, "tools", "gogen");
  const {emitGo} = await import(pathToFileURL(join(gogen, "emit-go.mjs")).href);
  const p = await compile();
  // a private copy of gogen's Go module: nothing is written into the checkout
  const mod = join(work, "go");
  rmSync(mod, {recursive: true, force: true});
  mkdirSync(join(mod, "cmd", "o4drec"), {recursive: true});
  for (const f of ["go.mod", "go.sum"]) cpSync(join(gogen, "go", f), join(mod, f));
  cpSync(join(gogen, "go", "abap"), join(mod, "abap"), {recursive: true, filter: (f) => !f.endsWith("_test.go")});
  writeFileSync(join(mod, "cmd", "o4drec", "zz_generated.go"), emitGo(p));
  writeFileSync(join(mod, "cmd", "o4drec", "main.go"), readFileSync(join(here, "o4drec.go.txt"), "utf8"));
  execFileSync("gofmt", ["-w", join(mod, "cmd", "o4drec")]);
  const bin = join(work, "o4drec");
  execFileSync("go", ["build", "-trimpath", "-tags=libm", "-ldflags=-s -w", "-o", bin, "./cmd/o4drec"],
    {cwd: mod, stdio: "inherit", env: {...process.env, CGO_ENABLED: "1", GOPROXY: process.env.GOPROXY ?? "off"}});
  const file = join(work, "frames.ndjson");
  const t = performance.now();
  await new Promise((ok, fail) => {
    const child = spawn(bin, [], {stdio: ["ignore", "pipe", "inherit"]});
    child.stdout.pipe(createWriteStream(file));
    child.on("exit", (code) => (code === 0 ? ok() : fail(new Error(`o4drec exited ${code}`))));
  });
  console.log(`recorded ${file} in ${((performance.now() - t) / 1000).toFixed(1)} s (osg ${osgCommit()}, -tags=libm)`);
}

/* ------------------------------------------------------------------ pack */
// The packed frames are about 120-160 MB (tools/pages/README.md), too much
// for this repository's history: they go to --rec-out (default <work>/rec),
// and the Pages workflow takes them from the branch named there.
async function packFrames() {
  const file = join(work, "frames.ndjson");
  if (!existsSync(file)) throw new Error(`${file} missing: run --only record first`);
  const out = resolve(arg("--rec-out", join(work, "rec")));
  rmSync(out, {recursive: true, force: true});
  await pack(file, out);
}

if (!only || only === "live") await live();
if (!only || only === "record") await record();
if (!only || only === "pack") await packFrames();
