# Vivid Vibes

**Real-time demoscene effects running in SAP ABAP**

[![Demo Video](https://img.youtube.com/vi/4IAPLbqJ2Eo/maxresdefault.jpg)](https://youtu.be/4IAPLbqJ2Eo)

> Click the image above to watch the demo in action

---

## What is this?

Vivid Vibes is a demoscene engine written entirely in ABAP. It renders classic demo effects server-side and streams them to the browser via WebSockets (ABAP Push Channel). The browser renders vector primitives in real-time, synchronized to music.

```
┌─────────────┐     ┌─────────────┐     ┌─────────────┐
│   ABAP      │ --> │  WebSocket  │ --> │  Browser    │
│   Server    │     │   (JSON)    │     │  (Canvas)   │
└─────────────┘     └─────────────┘     └─────────────┘
  Vector math         Streaming          Rendering
  coordinates         primitives         + audio sync
```

---

## Effects

| Effect | Class | Description |
|--------|-------|-------------|
| Starfield | `ZCL_O4D_STARFIELD` | 3D hyperspace starfield |
| Plasma | `ZCL_O4D_PLASMA` | Classic sine-wave plasma |
| Tunnel | `ZCL_O4D_TUNNEL` | Infinite tunnel with rings |
| Metaballs | `ZCL_O4D_METABALLS` | Organic blob shapes |
| Copper Bars | `ZCL_O4D_COPPERBARS` | Amiga-style gradient bars |
| Joy Division | `ZCL_O4D_JOYDIVISION` | Unknown Pleasures mountains |
| Elite 3D | `ZCL_O4D_ELITE3D` | Wireframe Cobra Mk III |
| Glenz | `ZCL_O4D_GLENZ` | Transparent vector objects |
| Twister | `ZCL_O4D_TWISTER` | 3D rotating text |
| Rotozoom | `ZCL_O4D_ROTOZOOM` | Rotating/zooming texture |
| Greetings | `ZCL_O4D_GREETINGS` | Sine wave text scroller |
| Fractals | `ZCL_O4D_MANDELBULB` | Mandelbrot, Julia, Burning Ship |

...and 30+ more effects!

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

**OISEE + Claude // 2025**

*Demoscene meets Enterprise!*

---

## License

MIT
