# Vivid Vibes

**Probably the first ever demo for SAP ABAP**

*Real-time demoscene effects running in SAP ABAP*

**Play it in the browser: https://oisee.github.io/vivid-vibes/** (no SAP system: the ABAP compiled to JavaScript runs in the page, or frames recorded from it play back; see [tools/pages/README.md](tools/pages/README.md))

[![Demo Video](./media/ZO4D_18_GLITCH.png)](https://youtu.be/4IAPLbqJ2Eo)

> Click the image above to watch the demo in action

---

## What is this?

Vivid Vibes is a demoscene engine written entirely in ABAP. It renders classic demo effects server-side and streams them to the browser via WebSockets (ABAP Push Channel). The browser renders vector primitives in real-time, synchronized to music.

### Architecture

```
┌─────────────────────┐         ┌─────────────────────┐
│       ABAP          │         │      Browser        │
│   "Scene Engine"    │         │  "Video Processor"  │
├─────────────────────┤  JSON   ├─────────────────────┤
│ - Effect logic      │ ─────►  │ - Canvas 2D         │
│ - 3D math           │ WebSocket│ - WebGL shaders    │
│ - Scene transitions │         │ - Audio sync        │
│ - Timeline/BPM sync │         │ - CRT post-FX       │
└─────────────────────┘         └─────────────────────┘
     Calculates                      Renders
     what to draw                    the frames
```

**ABAP** does all the heavy lifting: 3D transformations, effect calculations, scene orchestration, and beat synchronization. It outputs draw commands (lines, circles, polygons, text) as JSON.

**JavaScript** (Canvas/WebGL) acts as a "video processor" - it receives the primitives and renders them to screen. It also handles audio playback, post-processing effects (CRT scanlines, glow), and WebGL shaders for GPU-accelerated effects.

---

## Effects

| Preview | Effect | Class |
|:-------:|--------|-------|
| ![](media/ZO4D_06_PLASMA.png) | **Plasma** | `ZCL_O4D_PLASMA` |
| ![](media/ZO4D_05_COPPER.png) | **Copper Bars** | `ZCL_O4D_COPPERBARS` |
| ![](media/ZO4D_09_ROTOZOOM.png) | **Rotozoom** | `ZCL_O4D_ROTOZOOM` |
| ![](media/ZO4D_17_AMIGABALL.png) | **Amiga Ball** | `ZCL_O4D_AMIGABALL` |
| ![](media/ZO4D_07_TWISTER.png) | **Twister** | `ZCL_O4D_TWISTER` |
| ![](media/ZO4D_24_METABALLS.png) | **Metaballs** | `ZCL_O4D_METABALLS` |
| ![](media/ZO4D_21_JOYDIV.png) | **Joy Division** | `ZCL_O4D_JOYDIVISION` |
| ![](media/ZO4D_08_MOUNTAINS.png) | **Mountains** | `ZCL_O4D_MOUNTAINS` |
| ![](media/ZO4D_10_VOXEL.png) | **Voxel Landscape** | `ZCL_O4D_VOXEL_LANDSCAPE` |
| ![](media/ZO4D_12_TESSERACT.png) | **Tesseract (4D)** | `ZCL_O4D_TESSERACT` |
| ![](media/ZO4D_13_CELL24.png) | **24-Cell Polytope** | `ZCL_O4D_CELL24` |
| ![](media/ZO4D_15_CELL120.png) | **120-Cell Polytope** | `ZCL_O4D_CELL120` |
| ![](media/ZO4D_25_TORUS.png) | **Torus** | `ZCL_O4D_TORUS` |
| ![](media/ZO4D_23_QUATJULIA.png) | **Quaternion Julia** | `ZCL_O4D_QUATJULIA` |
| ![](media/ZO4D_26_JULIAMORPH.png) | **Julia Morph** | `ZCL_O4D_JULIA_MORPH` |
| ![](media/ZO4D_19_SIERPINSKI.png) | **Sierpinski** | `ZCL_O4D_SIERPINSKI` |
| ![](media/ZO4D_20_NEONCITY.png) | **Neon City** | `ZCL_O4D_NEONCITY` |
| ![](media/ZO4D_27_CONSTELL.png) | **Constellation** | `ZCL_O4D_CONSTELLATION` |
| ![](media/ZO4D_02_IGNITE.png) | **Ignition** | `ZCL_O4D_IGNITION` |
| ![](media/ZO4D_18_GLITCH.png) | **Glitch** | `ZCL_O4D_GLITCH` |

...and more effects!

---

## Installation

### 1. Install the package

Clone and import using [abapGit](https://abapgit.org/):

```
https://github.com/oisee/vivid-vibes
```

### 2. Activate the HTTP Handler

1. Go to transaction **SICF**
2. Create/activate service node: `/sap/bc/zo4d_demo`
3. Assign handler class: `ZCL_O4D_HTTP_HANDLER`

### 3. Activate the APC Handler

1. Go to transaction **SAPC** (or SE80 -> Create APC Application)
2. Create and activate the ABAP Push Channel service
3. Assign handler class: `ZCL_O4D_APC_HANDLER`

---

## Running the Demo

Open your browser and navigate to:

```
http://<your-sap-host>:<port>/sap/bc/zo4d_demo?player=megademo
```

Example:
```
http://vhcala4hci:50000/sap/bc/zo4d_demo?player=megademo
```

---

## Architecture

### Core Classes

| Class | Description |
|-------|-------------|
| `ZIF_O4D_EFFECT` | Effect interface with render context, timing, primitives |
| `ZCL_O4D_DEMO` | Scene orchestrator with transitions |
| `ZCL_O4D_COMPOSER` | Multi-scene timeline management |
| `ZCL_O4D_HTTP_HANDLER` | Serves the HTML5 player |
| `ZCL_O4D_APC_HANDLER` | WebSocket handler for real-time streaming |

### Rendering

- **Output**: JSON primitives (lines, circles, polygons, text)
- **Resolution**: 640x400 scalable vectors
- **Frame rate**: ~20-60 FPS (configurable via `fpt` parameter)
- **Timing**: Beat-synced at 76 BPM (configurable)

---

## Related Projects

### Vibing Steampunk

A complete demo production built with this framework:

**[github.com/oisee/vibing-steampunk](https://github.com/oisee/vibing-steampunk)**

---

## Credits

**Code**: OISEE + Claude // 2025

**Music**:
- *EA Rulez!* by Oisee (AY-8910 1999 / rmx 2025)
- *Ole Lukøjle* by Oisee (AY-8910 2001 / rmx 2025)

*Demoscene meets Enterprise!*

---

## License

MIT
