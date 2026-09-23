// The live mode: one APC connection of ZCL_O4D_APC_HANDLER, played by the
// demo's ABAP compiled to plain JS (live/demo.mjs, gogen). This worker is the
// APC framework's part: a handler per socket, ON_START, each message to
// ON_MESSAGE, ON_CLOSE, and a message manager whose SEND posts to the page.
// A dump ends the dialog step, as on a system, and is reported once.
import * as m from "../live/demo.mjs";

let s = null;
let h = null;
let buffer = null; // what ON_START sends, held until the page has seen open
let quiet = false;
const dumps = new Set();

const mm = {
  IF_APC_WSP_MESSAGE_MANAGER__CREATE_MESSAGE: () => message(""),
  IF_APC_WSP_MESSAGE_MANAGER__SEND: (_s, msg) => {
    if (quiet) return;
    const text = msg.IF_APC_WSP_MESSAGE__GET_TEXT(_s);
    if (buffer) buffer.push(text);
    else postMessage({op: "msg", text});
  },
  IF_APC_WSP_MESSAGE_MANAGER__SET_SEND_MODE: () => {},
};
function message(text) {
  const msg = {text};
  msg.IF_APC_WSP_MESSAGE__GET_TEXT = () => msg.text;
  msg.IF_APC_WSP_MESSAGE__SET_TEXT = (_s, v) => { msg.text = v; };
  msg.IF_APC_WSP_MESSAGE__GET_BINARY = () => msg.text;
  msg.IF_APC_WSP_MESSAGE__SET_BINARY = (_s, v) => { msg.text = v; };
  msg.IF_APC_WSP_MESSAGE__GET_MESSAGE_TYPE = () => 1;
  return msg;
}
const cx = {};
for (const k of ["IF_APC_WSP_SERVER_CONTEXT", "IF_APC_WSP_SERVER_CONTEXT_BASE"]) {
  cx[`${k}__GET_CONNECTION_ID`] = () => "1";
  cx[`${k}__GET_INITIAL_REQUEST`] = () => null;
  cx[`${k}__GET_BINDING_MANAGER`] = () => null;
}

function step(name, f) {
  try { f(); } catch (e) {
    const text = `dump in ${name}: ${e?.message ?? e}`;
    if (!dumps.has(text)) { dumps.add(text); postMessage({op: "dump", text}); }
  }
}
const onMessage = (text) => step("ON_MESSAGE", () => h.IF_APC_WSP_EXTENSION__ON_MESSAGE(s, message(text), mm, cx));

onmessage = (e) => {
  const d = e.data;
  if (d.op === "start") {
    s = {sy: {index: 0, tabix: 0, subrc: 0}};
    h = m.ZCL_O4D_APC_HANDLER.$new(s);
    buffer = [];
    quiet = !!d.silent; // a swap of workers under an open socket: the page has its config
    step("ON_START", () => h.IF_APC_WSP_EXTENSION__ON_START(s, cx, mm));
    for (const t of d.replay ?? []) onMessage(t);
    quiet = false;
    // open before what ON_START pushed, or the page answers a socket still CONNECTING
    postMessage({op: "open"});
    for (const text of buffer) postMessage({op: "msg", text});
    buffer = null;
  } else if (d.op === "send") {
    const t0 = performance.now();
    onMessage(d.text);
    postMessage({op: "done", frame: d.frame, ms: performance.now() - t0});
  } else if (d.op === "close") {
    if (h) step("ON_CLOSE", () => h.IF_APC_WSP_EXTENSION__ON_CLOSE(s, "", 1000, cx));
    h = null;
  } else if (d.op === "probe") {
    // how fast this device renders a heavy scene: a handler of its own, nothing sent
    s = {sy: {index: 0, tabix: 0, subrc: 0}};
    h = m.ZCL_O4D_APC_HANDLER.$new(s);
    quiet = true;
    step("ON_START", () => h.IF_APC_WSP_EXTENSION__ON_START(s, cx, mm));
    onMessage(JSON.stringify({cmd: "load_demo", demo: d.demo}));
    const ms = [];
    for (let k = 0; k < d.count; k++) {
      const t0 = performance.now();
      onMessage(JSON.stringify({cmd: "frame", tick: d.tick + k, sub: 0}));
      ms.push(performance.now() - t0);
    }
    postMessage({op: "probe", ms});
  }
};
