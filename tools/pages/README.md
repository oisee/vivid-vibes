# The GitHub Pages build

`pages/` is the demo as a static site: https://oisee.github.io/vivid-vibes/

The player pages are the ones `ZCL_O4D_HTTP_HANDLER` serves (the megademo at
`index.html`, the dev player at `dev-main.html` / `dev-outro.html`), with one
line added at the top of `<head>`: `vv/shim.js`. The shim answers the
player's WebSocket to `/sap/bc/apc/sap/zo4d_demo` in the page instead of APC,
and maps the handler's `?audio=` / `?img=` / `?image=` URLs to `media/`.
There are two modes, with a LIVE / RECORDED switch in the top right corner:

- **live** (the default): the demo's ABAP (`ZCL_O4D_APC_HANDLER`,
  `ZCL_O4D_DEMO`, every effect) compiled ahead of time to plain synchronous
  JavaScript (`pages/live/demo.mjs`, 813 KB, and its runtime
  `pages/live/abap.mjs`, 35 KB) runs in a Web Worker, one worker per socket:
  `ON_START`, each message to `ON_MESSAGE`, `ON_CLOSE`, and a message manager
  whose `SEND` posts to the page (`pages/vv/live-worker.js`). The class data
  of `ZCL_O4D_DEMO` is per worker, so the dev player's PRELOAD ALL, which
  opens several sockets, gets several workers.
- **recorded**, for devices that cannot keep the frame rate: every frame of
  the timeline recorded from the Go build of the same ABAP (sin and cos from
  glibc, the build that equals A4H frame for frame), replayed by tick
  (`pages/vv/replay-worker.js`). The player asks for a frame by the music's
  clock, so answering by tick keeps it in step with the music. Frames come in
  chunks of one bar (64 frames), fetched when first needed and two ahead.

The page starts live and switches itself to recorded (only when the recorded
frames are published, see below) when either measurement says the device is
too slow: a probe, a separate worker rendering 12 frames of the heaviest
scene (julia_morph; about 45-50 ms a frame in desktop Chromium, the limit is 4
frame intervals, 99 ms, `?probe-limit=<ms>` overrides it), or the first 64
frames played (median above one interval, 24.7 ms). `?mode=live` or
`?mode=recorded` pins a mode; the switch does as well.

A frame request waiting for the worker is replaced by a newer one, so a
worker slower than the music drops frames instead of falling behind it.

## What is published where

| | size | where |
| --- | ---: | --- |
| `pages/live/` (JS from gogen) | 848 KB | this repository |
| players + shim + workers | 110 KB | this repository |
| media (3 MP3, 25 PNG, from `src/*.w3mi.data.*`) | 11.9 MB | copied from `src/` by `tools/pages/site.sh` when the site is assembled |
| recorded frames | **144.9 MB** | **not in this repository**: branch `recorded-frames`, when it exists |

The recorded frames, measured (main part, 7680 frames at 40.5 fps):
1277 MB of JSON; 144.9 MB as shipped (per chunk the smaller of gzip over the
text or gzip over a token delta against the previous frame, `pages/vv/rec-codec.js`,
decoded with `DecompressionStream`). Alternatives measured on the same data:
gzip alone 159 MB, brotli (q9, 16 MB window) 125 MB, token delta + brotli 142
MB. Most of it is four scenes whose frames are thousands of 17-digit floats
that change every frame: quat_julia 50.7 MB, sierpinski 36.1 MB, rotozoom
14.0 MB, julia_morph 13.6 MB. The largest chunk is 7.1 MB.

Per scene (MB): sales_dance 0.10, ignition 0.44, ignite_emit 0.35,
copperbars 0.36, plasma 0.16, twistzoomer 2.78, mountains_oops 2.08,
rotozoom 13.98, voxel_landscape 5.65, rotozoom_plasma 6.02, tesseract 0.12,
cell24 0.13, cell16 0.01, cell120 0.56, amiga_ball 0.93, amiga_ball_2 0.49,
glitch 0.03, sierpinski 36.14, neon_city 3.98, joydivision 0.34,
sierpinski_tet 2.37, quat_julia 50.69, sdf_blobs 0.33, torus_3d 0.33,
julia_morph 13.57, constellation 2.94.

That is too much for `main`'s history, so `pages/rec/index.json` is a
placeholder and the page offers live only. To publish them, put the output of
`build.mjs --only pack` at the root of an orphan branch `recorded-frames`:

```
git switch --orphan recorded-frames
cp -R <work>/rec/. .
git add index.json main outro && git commit -m "Recorded frames" && git push origin recorded-frames
```

The Pages workflow copies that branch into `rec/` of the site, so the
frames come from the page's own origin. A release asset would not do: its
download redirects to another host, and neither answer carries CORS headers
(checked 2026-09-23), so the page could not fetch it. A new recording
replaces the branch's one commit rather than adding to it.

## The outro does not play

Every frame of the second part (`outro`, "VIVID VIBES - CREDITS") dumps in
both builds: `NOT_COMPILED in ZCL_O4D_GALLERY=>INT_TO_HEX
(zcl_o4d_gallery.clas.abap:411): conversion x -> string`. gogen does not
compile that conversion yet. The recorder records the dumps as frames that do
not exist, and the page answers nothing for them, as a system would after a
short dump. The live worker reports each distinct dump once as a console
warning.

## How it is built

Not in this repository's CI: the compiler is gogen, `tools/gogen` of
open-steamgate on the branch `spike/go-backend`, which is not released.

What the committed `pages/` was built from:

| input | commit |
| --- | --- |
| open-steamgate, branch `spike/go-backend` (`tools/gogen`) | `98d4d1cd9b5ae4f8460590c85fa7592608e41f99` |
| open-abap-core (`github.com/oisee/open-abap-core`, branch `w3mimetabtype`) | `4eec77739655923e1f93a386de514fdd04d12523` |
| open-abap-apc (`github.com/oisee/open-abap-apc`) | `3cb371b641de5c12ee948b45b026cd92ed6ebafa` |
| this repository's `src/` | the commit named in `pages/BUILD.json` |

Node 22 or later with open-steamgate's `npm ci` done (gogen uses its
`@abaplint/core` and `@abaplint/transpiler`), and for the recording Go 1.26
with cgo.

```
# the JS, the players, BUILD.json -> pages/
node tools/pages/build.mjs --only live \
  --osg <open-steamgate at 98d4d1c> --core <open-abap-core at 4eec777>/src --apc <open-abap-apc at 3cb371b>/src

# the recording (Go, -tags=libm) -> <work>/frames.ndjson (1.5 GB), then the chunks -> <work>/rec
node tools/pages/build.mjs --only record --osg ... --core ... --apc ... --work <work>
node tools/pages/build.mjs --only pack --work <work>
```

`record` copies gogen's Go module into `<work>/go` and builds there; nothing
is written into the open-steamgate checkout. `--pure --max <n>` records the
first n frames of each part with gogen's default build instead (fdlibm sin
and cos) into `<work>/frames-pure.ndjson`, for comparisons.

## How it was checked

```
tools/pages/site.sh <site> <work>/rec      # pages/ + media/ + rec/
# serve <site> with any static server that answers Range requests
node <vivid-vibes>/tools/pages/check.mjs --url http://127.0.0.1:3197/ \
  --frames <work>/frames.ndjson --frames-pure <work>/frames-pure.ndjson   # from open-steamgate (Playwright)
```

In headless Chromium 151: both modes play eight scenes of the main part
(the audio moved to each scene's first bar, 32-75 frames each in 2.5 s) with
no console error, page error or failed request; the LIVE / RECORDED switch
swaps the worker under an open socket while playing; the dev player's PRELOAD
ALL runs five sockets on five workers; the default page stays live on a
desktop and falls back to recorded when the probe's limit is lowered.

Live frames against the Go recording (libm, equal to A4H), frame by frame on
fresh sockets: ticks 0-2047 1948/2048 byte-equal, and 64/64 from the start of
amiga_ball, sierpinski and julia_morph. The 100 others differ in the last bit
of a number (relative difference at most 4.3e-16: sales_dance 1,
twistzoomer 37, mountains_oops 14) or, in ignition (46) and ignite_emit (2),
in the particles, which are seeded from sin chains. Chromium's V8 is closer
to glibc than gogen's pure-Go default (fdlibm): against that build 1269/2048
are byte-equal.
