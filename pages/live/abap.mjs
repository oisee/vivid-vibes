// The runtime of JS emitted from the gogen IR (tools/gogen/emit-js.mjs): the
// same semantics as go/abap, line for line, with plain JS values. i and f
// are numbers, c / x / string are strings, a structure is an object and a
// table an array. Nothing here tests a type at run time: the IR decided.

export class AbapError extends Error {
  constructor(cls, op) {
    super(`${cls} in ${op}`);
    this.cls = cls;
  }
}

const MAX = 2147483647;
const MIN = -2147483648;
const check = (v, op) => {
  if (v > MAX || v < MIN) throw new AbapError("CX_SY_ARITHMETIC_OVERFLOW", op);
  return v;
};

export const AddI = (a, b) => check(a + b, "+");
export const SubI = (a, b) => check(a - b, "-");
export const MulI = (a, b) => check(a * b, "*");
export const NegI = (a) => check(-a, "-");

// `/` with calculation type i: the quotient rounded half away from zero,
// and 0 / 0 = 0
export function DivI(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "/");
  }
  const r = a % b;
  let q = (a - r) / b;
  if (2 * Math.abs(r) >= Math.abs(b)) q += (a < 0) !== (b < 0) ? -1 : 1;
  return check(q, "/");
}

// DIV and MOD keep the remainder in [0, |b|)
export function DivIntI(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "DIV");
  }
  let r = a % b;
  if (r < 0) r += Math.abs(b);
  return check((a - r) / b, "DIV");
}

export function ModI(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "MOD");
  }
  let r = a % b;
  if (r < 0) r += Math.abs(b);
  return r;
}

export function DivF(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "/");
  }
  return a / b;
}

// for f, the quotient that leaves a - b * q non-negative, and MOD computed
// from it: what A4H does (abaplint/transpiler#1885)
const quotF = (a, b) => (b > 0 ? Math.floor(a / b) : -Math.floor(a / -b));
export function DivIntF(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "DIV");
  }
  return quotF(a, b);
}
export function ModF(a, b) {
  if (b === 0) {
    if (a === 0) return 0;
    throw new AbapError("CX_SY_ZERODIVIDE", "MOD");
  }
  const v = a - b * quotF(a, b);
  return v === 0 ? 0 : v;
}

// f -> i: half away from zero, and out of range is an overflow
export function F2I(f) {
  const r = f < 0 ? -Math.round(-f) : Math.round(f);
  if (Number.isNaN(r) || r > MAX || r < MIN) throw new AbapError("CX_SY_CONVERSION_OVERFLOW", "f->i");
  return r === 0 ? 0 : r;
}

export const AbsI = (a) => check(Math.abs(a), "abs");
export const SignI = (v) => (v > 0 ? 1 : v < 0 ? -1 : 0);
export const SignF = SignI;
export const FracF = (v) => v - Math.trunc(v);
export const MaxI = Math.max;
export const MinI = Math.min;
export const MaxF = Math.max;
export const MinF = Math.min;
export function SqrtF(v) {
  if (v < 0) throw new AbapError("CX_SY_ARG_OUT_OF_DOMAIN", "sqrt");
  return Math.sqrt(v);
}
export function LogF(v) {
  if (v <= 0) throw new AbapError("CX_SY_ARG_OUT_OF_DOMAIN", "log");
  return Math.log(v);
}

// character-like values: a c field is stored without its trailing blanks
export function CFit(v, n) {
  const chars = [...v];
  return (chars.length > n ? chars.slice(0, n).join("") : v).replace(/ +$/, "");
}
export const FmtI = (v) => String(v);
export function IToX(v, n) {
  const b = [(v >>> 24) & 255, (v >>> 16) & 255, (v >>> 8) & 255, v & 255];
  const out = n <= 4 ? b.slice(4 - n) : [...new Array(n - 4).fill(v < 0 ? 255 : 0), ...b];
  return String.fromCharCode(...out);
}
export const XToHex = (v) => [...v].map((c) => c.charCodeAt(0).toString(16).padStart(2, "0")).join("").toUpperCase();
export function ParseF(v) {
  let t = v.trim();
  if (t === "") return 0;
  let neg = false;
  if (t.endsWith("-")) { neg = true; t = t.slice(0, -1).trim(); }
  const f = Number(t);
  if (Number.isNaN(f)) throw new AbapError("CX_SY_CONVERSION_NO_NUMBER", "c->f");
  return neg ? -f : f;
}
export const ParseI = (v) => F2I(ParseF(v));
export const ToUpper = (v) => v.toUpperCase();
export const ToLower = (v) => v.toLowerCase();
export const Strlen = (v) => [...v].length;

// f in a string template, as measured on A4H: seventeen significant digits,
// positional, trailing zeros of the fraction dropped
export function FmtF(v) {
  if (v === 0) return "0";
  const neg = v < 0;
  const [mant, exp] = Math.abs(v).toExponential(16).split("e");
  const digits = mant.replace(".", "");
  const point = Number(exp) + 1;
  let intPart;
  let frac;
  if (point <= 0) { intPart = "0"; frac = "0".repeat(-point) + digits; }
  else if (point >= digits.length) { intPart = digits + "0".repeat(point - digits.length); frac = ""; }
  else { intPart = digits.slice(0, point); frac = digits.slice(point); }
  frac = frac.replace(/0+$/, "");
  return (neg ? "-" : "") + intPart + (frac === "" ? "" : `.${frac}`);
}

// value semantics: a structure or table moved out of a place is copied
export function copy(v) {
  if (Array.isArray(v)) return v.map(copy);
  // a structure is a plain object and is copied; an object of a class is a
  // reference and is shared, as ABAP moves a TYPE REF TO
  if (v !== null && typeof v === "object" && Object.getPrototypeOf(v) === Object.prototype) {
    const o = {};
    for (const k in v) o[k] = copy(v[k]);
    return o;
  }
  return v;
}

export function Idx(n, i) {
  if (i < 1 || i > n) throw new AbapError("CX_SY_ITAB_LINE_NOT_FOUND", "table expression");
  return i - 1;
}

export function PowF(a, b) {
  if (a < 0 && b !== Math.trunc(b)) throw new AbapError("CX_SY_ARG_OUT_OF_DOMAIN", "**");
  if (a === 0 && b < 0) throw new AbapError("CX_SY_ZERODIVIDE", "**");
  return a ** b;
}

// WIDTH / ALIGN / PAD, as measured on A4H: padded, never cut
export function Pad(v, width, align, pad) {
  const n = [...v].length;
  if (n >= width) return v;
  const fill = width - n;
  if (align === "RIGHT") return pad.repeat(fill) + v;
  if (align === "CENTER") return pad.repeat(fill >> 1) + v + pad.repeat(fill - (fill >> 1));
  return v + pad.repeat(fill);
}

// DECIMALS = n of an f, as measured on A4H (see go/abap FmtFDec)
export function FmtFDec(v, n) {
  const neg = v < 0 || Object.is(v, -0);
  const a = Math.abs(v);
  let intPart;
  let frac;
  if (a >= 1e21) { intPart = FmtF(a); frac = "0".repeat(100); }
  else { [intPart, frac] = a.toFixed(100).split("."); }
  const digits = intPart.replace(/^0+/, "").length;
  if (digits > 0 && n > 17 - digits) n = Math.max(0, 17 - digits);
  const b = (intPart + frac.slice(0, n)).split("");
  if (frac[n] >= "5") {
    let i = b.length - 1;
    for (; i >= 0; i--) {
      if (b[i] === "9") { b[i] = "0"; continue; }
      b[i] = String(Number(b[i]) + 1);
      break;
    }
    if (i < 0) b.unshift("1");
  }
  const s = b.join("");
  const ip = s.slice(0, s.length - n).replace(/^0+/, "") || "0";
  return (neg ? "-" : "") + ip + (n > 0 ? `.${s.slice(s.length - n)}` : "");
}

// BIT-AND / BIT-OR / BIT-XOR of two x fields of one length
export function BitX(op, a, b) {
  let out = "";
  for (let i = 0; i < a.length; i++) {
    const x = a.charCodeAt(i); const y = b.charCodeAt(i);
    out += String.fromCharCode(op === "BIT-AND" ? x & y : op === "BIT-OR" ? x | y : x ^ y);
  }
  return out;
}
// an x of fewer than four bytes into an i: 00 on the left, read unsigned
export function XToI(v) {
  let r = 0;
  for (let i = 0; i < v.length; i++) r = r * 256 + v.charCodeAt(i);
  return r;
}

const rangeError = () => { throw new AbapError("CX_SY_RANGE_OUT_OF_BOUNDS", "offset/length"); };
// v+off(len) of a string in characters; len -1 is the rest; out of range raises
export function SubS(v, off, len) {
  const r = [...v];
  if (off < 0 || off > r.length) rangeError();
  if (len < 0) return r.slice(off).join("");
  if (off + len > r.length) rangeError();
  return r.slice(off, off + len).join("");
}
// v+off(len) of a c field of length n: read padded, stored trimmed
export const SubC = (v, n, off, len) => SubS(v.padEnd(n, " ").slice(0, n), off, len).replace(/ +$/, "");
// bytes of an x or xstring (one char per byte)
export function SubX(v, off, len) {
  if (off < 0 || off > v.length) rangeError();
  if (len < 0) return v.slice(off);
  if (off + len > v.length) rangeError();
  return v.slice(off, off + len);
}
export const XFit = (v, n) => (v.length >= n ? v.slice(0, n) : v + "\u0000".repeat(n - v.length));
export const Uccpi = (v) => String.fromCodePoint(v).replace(/ +$/, "");
// find( val sub off ), as measured on A4H: offset or -1, empty sub raises
export function Find(v, sub, off) {
  if (sub === "") throw new AbapError("CX_SY_STRG_PAR_VAL", "find");
  const r = [...v];
  if (off < 0 || off > r.length) rangeError();
  const rest = r.slice(off).join("");
  const i = rest.indexOf(sub);
  return i < 0 ? -1 : off + [...rest.slice(0, i)].length;
}
export const CO = (a, b) => [...a].every((c) => b.includes(c));
export const CS = (a, b) => b === "" || a.toUpperCase().includes(b.toUpperCase());
// i into a string, as A4H moves it: 42 is "42 ", -5 is "5-"
export const IToString = (v) => (v < 0 ? `${-v}-` : `${v} `);
// code point of a character; the blank c, stored empty, is 32
export const Uccp = (v) => (v.length === 0 ? 32 : v.codePointAt(0));
// SPLIT ... INTO TABLE as A4H does it
export function Split(v, sep) {
  if (v === "") return [];
  // an empty separator does not split (measured: abc is one row)
  if (sep === "") return [v];
  const parts = v.split(sep);
  if (v.endsWith(sep)) parts.pop();
  return parts;
}
// an unseeded cl_abap_random_int: any number in [min, max]
let seeded = false;
let state = 0;
// harnesses only: the same xorshift32 as the Go runtime
export function SeedRandom(seed) { seeded = true; state = seed >>> 0; }
export function RandomInt(min, max) {
  if (!seeded) return min + Math.floor(Math.random() * (max - min + 1));
  state ^= state << 13; state >>>= 0;
  state ^= state >>> 17;
  state ^= state << 5; state >>>= 0;
  return min + (state % (max - min + 1));
}
// FIND [REGEX] p IN s: [found, offset, length, submatches]. A JS RegExp is
// leftmost-first where ABAP's POSIX is leftmost-longest: an alternation whose
// shorter branch matches first (a|ab) differs; the Go runtime is the exact one.
export function FindStmt(s, p, regex, icase, n) {
  const subs = new Array(n).fill("");
  if (!regex) {
    if (p === "") return [false, 0, 0, subs];
    const i = (icase ? s.toUpperCase() : s).indexOf(icase ? p.toUpperCase() : p);
    return i < 0 ? [false, 0, 0, subs] : [true, [...s.slice(0, i)].length, [...p].length, subs];
  }
  // non-greedy is invalid on A4H; (?:...) and lookahead are valid there and here
  if (/\*\?|\+\?|\?\?/.test(p)) throw new AbapError("CX_SY_INVALID_REGEX", p);
  const m = abapRegExp(p, s, icase, "", "FIND REGEX").exec(s);
  if (!m) return [false, 0, 0, subs];
  for (let i = 0; i < n; i++) subs[i] = m[i + 1] ?? "";
  return [true, [...s.slice(0, m.index)].length, [...m[0]].length, subs];
}

// An ABAP regex as a JS RegExp, with the lines of the Go runtime: ^ and $
// are the start and end of a line and . is any character (A4H 2026-09-23),
// where lines end at \n only. JS's m flag also ends one at \r, U+2028 and
// U+2029, so the anchors are rewritten rather than flagged; an anchored
// pattern on a text holding one of those is NOT_COMPILED, as in Go
// (checkLines: A4H's own rule there is not Go's and not JS's).
function abapRegExp(p, s, icase, g, where) {
  let out = "", anchored = false;
  for (let i = 0; i < p.length; i++) {
    const c = p[i];
    if (c === "\\") { out += c + (p[i + 1] ?? ""); i++; continue; }
    if (c === "^") { out += "(?:^|(?<=\n))"; anchored = true; continue; }
    if (c === "$") { out += "(?:$|(?=\n))"; anchored = true; continue; }
    if (c !== "[") { out += c; continue; }
    let j = i + 1;
    if (p[j] === "^") j++;
    if (p[j] === "]") j++;
    for (; j < p.length && p[j] !== "]"; j++) {
      if (p[j] === "[" && ":=.".includes(p[j + 1] ?? "x")) {
        const end = p[j + 1];
        for (j += 2; j + 1 < p.length && !(p[j] === end && p[j + 1] === "]"); j++);
        j++;
      }
    }
    out += p.slice(i, j + 1);
    i = j;
  }
  if (anchored && /[\r\u2028\u2029]/.test(s)) throw nc(where, `^ or $ in a text with a line end other than \\n is not measured: ${p}`);
  try { return new RegExp(out, (icase ? "i" : "") + g + "su"); } catch { throw new AbapError("CX_SY_INVALID_REGEX", p); }
}

// CREATE OBJECT ... TYPE (name): every compiled class registers what it is
// (its own name and its interfaces) and a constructor without arguments. The
// fit is checked before the constructor runs, as in the kernel.
const classes = new Map();
export function registerClass(name, is, make) { classes.set(name, {is: new Set(is), make}); }
export function createAs(s, name, target) {
  const c = classes.get(String(name).replace(/ +$/, ""))  // as written: lower case is unknown (A4H);
  if (c === undefined) throw new AbapError("CX_SY_CREATE_OBJECT_ERROR", `CREATE OBJECT TYPE (${name})`);
  if (!c.is.has(target)) throw new AbapError("CX_SY_MOVE_CAST_ERROR", `CREATE OBJECT TYPE (${name})`);
  return c.make(s);
}

// ?= and CAST: an initial reference casts to initial; anything else must be
// the target class or interface (the class's $is), or CX_SY_MOVE_CAST_ERROR
export function cast(x, target) {
  if (x === null || x === undefined) return null;
  if (!x.constructor?.$is?.has(target)) throw new AbapError("CX_SY_MOVE_CAST_ERROR", "?=");
  return x;
}

// RAISE name, taken only by the caller's EXCEPTIONS list (see the Go runtime)
export class ClassicException extends Error {
  constructor(name, method) { super(`RAISE_EXCEPTION ${name} in ${method}`); this.exName = name; this.method = method; }
}
export function classic(s, e, method, map, others) {
  if (e instanceof ClassicException && e.method === method) {
    if (map[e.exName] !== undefined) { s.sy.subrc = map[e.exName]; return; }
    if (others !== 0) { s.sy.subrc = others; return; }
  }
  throw e;
}

// CP and CA: see the Go runtime (conv.go), measured on A4H
export function CP(a, p, cpat) {
  if (cpat && p === "") p = " ";
  const ps = [];
  const pr = [...p];
  for (let i = 0; i < pr.length; i++) {
    if (pr[i] === "#" && i + 1 < pr.length) ps.push({r: pr[++i], k: "e"});
    else if (pr[i] === "*") ps.push({k: "*"});
    else if (pr[i] === "+") ps.push({k: "+"});
    else ps.push({r: pr[i], k: "l"});
  }
  const ar = [...a];
  const eq = (t, c) => (t.k === "+" ? true : t.k === "e" ? t.r === c : t.r.toUpperCase() === c.toUpperCase());
  let i = 0, j = 0, star = -1, mark = 0;
  while (i < ar.length) {
    if (j < ps.length && ps[j].k !== "*" && eq(ps[j], ar[i])) { i++; j++; }
    else if (j < ps.length && ps[j].k === "*") { star = j; mark = i; j++; }
    else if (star >= 0) { j = star + 1; mark++; i = mark; }
    else return false;
  }
  while (j < ps.length && ps[j].k === "*") j++;
  return j === ps.length;
}
export function CA(a, b) { return b !== "" && [...a].some((c) => b.includes(c)); }

export function notCompiled(why) { throw new AbapError("NOT_COMPILED", why); }
export function Condense(s, noGaps) { return noGaps ? s.replaceAll(" ", "") : s.split(" ").filter((x) => x !== "").join(" "); }

// ---- strings (measured on A4H 2026-09-23, see go/abap/strings.go) ----
const nc = (where, why) => new AbapError("NOT_COMPILED", `${where}: ${why}`);
// a c value at its full length: SPLIT's separator keeps its trailing blanks
export const PadC = (v, n) => { const c = [...v].length; return c < n ? v + " ".repeat(n - c) : v; };
// SPLIT v AT sep INTO t1 .. tn: the last target takes the rest
export function SplitInto(v, sep, n) {
  const out = new Array(n).fill("");
  if (v === "") return out;
  if (sep === "") { out[0] = v; return out; }
  const parts = v.split(sep);
  for (let i = 0; i < n && i < parts.length; i++) out[i] = i === n - 1 ? parts.slice(i).join(sep) : parts[i];
  return out;
}
export const SplitSubrc = (pieces, lens) => (pieces.some((p, i) => lens[i] >= 0 && [...p].length > lens[i]) ? 4 : 0);
// every match of an ABAP regex, in REPLACE ALL's order (see rxAll in Go).
// A JS RegExp is leftmost-first where ABAP's POSIX is leftmost-longest, as
// in FindStmt: an alternation whose shorter branch matches first differs.
function rxAll(s, p, icase, first) {
  if (/\*\?|\+\?|\?\?/.test(p)) throw new AbapError("CX_SY_INVALID_REGEX", p);
  const re = abapRegExp(p, s, icase, "g", "REPLACE REGEX");
  const out = [];
  let pos = 0;
  while (pos <= s.length) {
    re.lastIndex = pos;
    const m = re.exec(s);
    if (!m) break;
    out.push(m);
    if (first) break;
    const end = m.index + m[0].length;
    if (end > m.index) { pos = end; if (pos === s.length) break; continue; }
    if (m.index >= s.length) break;
    pos = m.index + (s.codePointAt(m.index) > 0xffff ? 2 : 1);
  }
  return out;
}
function plainAll(s, sub, icase, first) {
  if (icase) return rxAll(s, sub.replace(/[.*+?^${}()|[\]\\]/g, "\\$&"), true, first);
  const out = [];
  for (let pos = 0; pos <= s.length;) {
    const i = s.indexOf(sub, pos);
    if (i < 0) break;
    const m = [sub]; m.index = i; out.push(m);
    if (first) break;
    pos = i + sub.length;
  }
  return out;
}
function rxWith(wth, m) {
  if (!/[$\\]/.test(wth)) return wth;
  const r = [...wth];
  let b = "";
  for (let i = 0; i < r.length; i++) {
    if (r[i] === "\\" && i + 1 < r.length) { b += r[++i]; continue; }
    if (r[i] === "$" && /[0-9]/.test(r[i + 1] ?? "")) {
      if (/[0-9]/.test(r[i + 2] ?? "")) throw nc("REPLACE REGEX", `a replacement $nn of two digits is not measured: ${wth}`);
      b += m[Number(r[++i])] ?? "";
      continue;
    }
    if (r[i] === "$" && r[i + 1] === "&") { b += m[0]; i++; continue; }
    if (r[i] === "$" || r[i] === "\\") throw nc("REPLACE REGEX", `a replacement ${r.slice(i).join("")} is not measured`);
    b += r[i];
  }
  return b;
}
function splice(s, ms, wth) {
  let b = "", last = 0;
  for (const m of ms) { b += s.slice(last, m.index) + wth(m); last = m.index + m[0].length; }
  return b + s.slice(last);
}
export const NoLength = -2147483648;
// REPLACE ... IN [SECTION ...] v WITH w; [value, sy-subrc]
export function ReplaceStmt(v, p, wth, regex, all, icase, off, ln, cLen) {
  if (cLen >= 0) v = PadC(v, cLen);
  const cs = [...v];
  const n = cs.length;
  if (ln === NoLength) ln = n - off;
  else if (ln < 0) throw nc("REPLACE", "a SECTION of negative LENGTH is not measured");
  if (off < 0 || off > n || off + ln > n) rangeError();
  const head = cs.slice(0, off).join(""), sec = cs.slice(off, off + ln).join(""), tail = cs.slice(off + ln).join("");
  let ms;
  if (regex) ms = rxAll(sec, p, icase, !all);
  else {
    if (p === "" && all) throw new AbapError("CX_SY_REPLACE_INFINITE_LOOP", "REPLACE ALL OCCURRENCES OF ''");
    ms = plainAll(sec, p, icase, !all);
  }
  if (ms.length === 0) return [cLen >= 0 ? v.replace(/ +$/, "") : v, 4];
  const out = head + splice(sec, ms, (m) => (regex ? rxWith(wth, m) : wth)) + tail;
  if (cLen >= 0) {
    const oc = [...out];
    if (oc.length > cLen) {
      return [CFit(out, cLen), oc.slice(cLen).join("").replace(/ +$/, "") === "" ? 0 : 2];
    }
    return [out.replace(/ +$/, ""), 0];
  }
  return [out, 0];
}
// replace( val sub|regex with occ )
export function ReplaceFn(v, p, wth, regex, occ) {
  if (p === "") {
    if (regex) throw nc("replace( )", "an empty regex is not measured");
    throw new AbapError("CX_SY_STRG_PAR_VAL", "replace");
  }
  let ms = regex ? rxAll(v, p, false, occ === 1) : plainAll(v, p, false, occ === 1);
  if (occ > 0) ms = occ <= ms.length ? [ms[occ - 1]] : [];
  else if (occ < 0) ms = -occ <= ms.length ? [ms[ms.length + occ]] : [];
  return splice(v, ms, (m) => (regex ? rxWith(wth, m) : wth));
}
export function Repeat(v, occ) {
  if (occ < 0) throw new AbapError("CX_SY_STRG_PAR_VAL", "repeat");
  return v.repeat(occ);
}
export function CondenseFn(v, del, from, to) {
  let r = [...v];
  while (r.length && del.includes(r[0])) r.shift();
  while (r.length && del.includes(r[r.length - 1])) r.pop();
  if (from === "") return r.join("");
  const rep = to === "" ? "" : [...to][0];
  let b = "", inRun = false;
  for (const c of r) {
    if (from.includes(c)) { if (!inRun) b += rep; inRun = true; continue; }
    inRun = false; b += c;
  }
  return b;
}
export function ShiftFn(v, left, kind, n, sub) {
  const r = [...v];
  const l = r.length;
  if (kind === "") return left ? v.replace(/^ +/, "") : v.replace(/ +$/, "");
  if (kind === "places") {
    if (n < 0 || n > l) rangeError();
    return (left ? r.slice(n) : r.slice(0, l - n)).join("");
  }
  if (kind === "circular") {
    if (n < 0) throw nc("shift( )", "a negative circular is not measured");
    if (n > l) rangeError();
    return left ? r.slice(n).join("") + r.slice(0, n).join("") : r.slice(l - n).join("") + r.slice(0, l - n).join("");
  }
  if (sub === "") throw nc("shift( )", "an empty sub is not measured");
  while (left && v.startsWith(sub)) v = v.slice(sub.length);
  while (!left && v.endsWith(sub)) v = v.slice(0, v.length - sub.length);
  return v;
}
export function ToMixed(v, sep, hasCase, cs, min) {
  if ([...sep].length !== 1) throw nc("to_mixed( )", "a sep that is not one character is not measured");
  if (min < 1) throw nc("to_mixed( )", "a min below 1 is not measured");
  const r = [...v];
  let b = "";
  for (let i = 0; i < r.length; i++) {
    if (i === 0) {
      if (!hasCase) b += r[0];
      else { const c = [...cs][0] ?? ""; b += c !== "" && c === c.toUpperCase() && c !== c.toLowerCase() ? r[0].toUpperCase() : r[0].toLowerCase(); }
    } else if (r[i] === sep && i >= min && i + 1 < r.length) b += r[++i].toUpperCase();
    else b += r[i].toLowerCase();
  }
  return b;
}

// RAISE EXCEPTION: the object and its class travel in a throw, beside the
// runtime's own AbapError (see raise.go)
export class Raised extends Error {
  constructor(obj, cls) { super(`UNCAUGHT_EXCEPTION ${cls}`); this.obj = obj; this.cls = cls; }
}
const supers = new Map();
export function registerSupers(m) { for (const [k, v] of Object.entries(m)) supers.set(k, v); }
export function isA(cls, ancestor) {
  for (let c = cls, n = 0; c && n < 40; c = supers.get(c), n += 1) if (c === ancestor) return true;
  return false;
}
export function raise(obj, cls) {
  if (obj === null || obj === undefined) throw new AbapError("OBJECTS_OBJREF_NOT_ASSIGNED", "RAISE EXCEPTION of an initial reference");
  const c = cls || obj.constructor?.$abap;
  if (!c) throw new AbapError("NOT_COMPILED", "RAISE EXCEPTION: the class of the object is not registered");
  return new Raised(obj, c);
}
// the CATCH clauses of the active TRYs of a session and Handled( ): see
// raise.go; a CLEANUP runs only when a TRY further out takes the exception
export function pushHandler(s, f) { const h = (s.handlers ??= []); const n = h.length; h.push(f); return n; }
export function popHandler(s, n) { if (s.handlers) s.handlers.length = n; }
export function handled(s, e) {
  const h = s.handlers ?? [];
  for (let i = h.length - 1; i >= 0; i--) if (h[i](e)) return true;
  return false;
}
export function classBased(e) {
  return e instanceof Raised || (e instanceof AbapError && e.cls.startsWith("CX_"));
}
// get_text( ) of a value a CATCH INTO received that also takes runtime
// exceptions: a raised object's own get_text, else the class and the operation
export function excText(s, x) {
  if (x instanceof Raised) {
    if (typeof x.obj.IF_MESSAGE__GET_TEXT !== "function") throw new AbapError("NOT_COMPILED", `${x.cls}=>GET_TEXT: get_text( ) of the class is not compiled`);
    return x.obj.IF_MESSAGE__GET_TEXT(s);
  }
  return x.message;
}
// Generic data: TYPE any, TYPE data, ANY TABLE and REF TO data, as in
// go/abap/data.go. A generic value is a binding to the slot it stands for,
// {get, set, t}: get reads the slot, set writes it, t is the descriptor of
// the slot's ABAP type. Never a copy, so a write through a field symbol or a
// data reference reaches the original. An unassigned field symbol and an
// initial reference are null. Descriptors of structures and tables are
// generated with the program ({kind, comps: [{name, key, t}], row, zero});
// elementary ones are here.
const sizedTypes = new Map();
const sizedType = (kind, len, dec = 0) => {
  const key = `${kind}:${len}:${dec}`;
  if (!sizedTypes.has(key)) sizedTypes.set(key, {kind, len, dec});
  return sizedTypes.get(key);
};
export const TI = {kind: "I", len: 4};
export const TInt8 = {kind: "8", len: 8};
export const TF = {kind: "F", len: 8};
export const TString = {kind: "g"};
export const TXString = {kind: "y"};
export const TD = {kind: "D", len: 8};
export const TT = {kind: "T", len: 6};
export const TRef = {kind: "l"};
export const TObj = {kind: "r"};
export const TC = (n) => sizedType("C", n);
export const TX = (n) => sizedType("X", n);
export const TP = (n, dec) => sizedType("P", n, dec);

// a value that is no place of its own, seen as generic data: a slot of its own
export function cell(v, t) {
  const c = {v};
  return {get: () => c.v, set: (x) => { c.v = x; }, t};
}

const notAssigned = (op) => new AbapError("GETWA_NOT_ASSIGNED", op);

// ASSIGN COMPONENT name OF STRUCTURE d: null (sy-subrc 4) when d is not a
// structure or has no component of that name; the name in any case (A4H).
// The component is reached through d each time, so it stays the field of
// whatever structure d's slot holds.
export function Component(d, name) {
  if (d === null || (d.t.kind !== "u" && d.t.kind !== "v")) return null;
  const n = String(name).replace(/ +$/, "").toUpperCase();
  const c = d.t.comps.find((x) => x.name === n);
  if (c === undefined) return null;
  return {get: () => d.get()[c.key], set: (v) => { d.get()[c.key] = v; }, t: c.t};
}

// lines( ) of a generic table
export function Lines(d) {
  if (d === null || d.t.kind !== "h") throw new AbapError("NOT_COMPILED", "lines( ): of a generic value that is not a table");
  return d.get().length;
}

// row i (from 0) of a generic table, bound to the row itself
export function Row(d, i) {
  const a = d.get();
  return {get: () => a[i], set: (v) => { a[i] = v; }, t: d.t.row};
}

// a generic elementary value moved into a string
export function DataString(d) {
  if (d === null) throw notAssigned("move");
  switch (d.t.kind) {
    case "g": case "C": case "D": case "T": case "N": return d.get();
    case "I": return IToString(d.get());
    default: throw new AbapError("NOT_COMPILED", `move: a generic value of type kind ${d.t.kind} into a string`);
  }
}

// a generic value moved into an i
export function DataI(d) {
  if (d === null) throw notAssigned("move");
  if (d.t.kind === "I") return d.get();
  throw new AbapError("NOT_COMPILED", `move: a generic value of type kind ${d.t.kind} into an i`);
}

// a generic value in a string template
export function FmtData(d) {
  if (d === null) throw notAssigned("string template");
  switch (d.t.kind) {
    case "I": return FmtI(d.get());
    case "8": return String(d.get());
    case "F": return FmtF(d.get());
    case "g": case "C": case "D": case "T": case "N": return d.get();
    case "X": case "y": return XToHex(d.get());
    default: throw new AbapError("NOT_COMPILED", `string template: a generic value of type kind ${d.t.kind}`);
  }
}

// IS INITIAL of a generic value
export function IsInitialData(d) {
  if (d === null) return true;
  const v = d.get();
  switch (d.t.kind) {
    case "I": case "F": return v === 0;
    case "8": return v === 0n;
    case "g": case "y": case "C": return v === "";
    case "D": return v === "" || v === "00000000";
    case "T": return v === "" || v === "000000";
    case "X": return /^\u0000*$/.test(v);
    case "P": return v.replaceAll(".", "").replace(/^[0-]+/, "") === "";
    case "h": return v.length === 0;
    case "u": case "v": return d.t.comps.every((c) => IsInitialData({get: () => v[c.key], t: c.t}));
    case "l": case "r": return v === null;
    default: throw new AbapError("NOT_COMPILED", `IS INITIAL: a generic value of type kind ${d.t.kind}`);
  }
}

// A structure or table written through generic data is written in place, as
// Go writes through the pointer: the object in the slot stays the same
// object, so a typed field symbol holding it (LOOP ASSIGNING, READ TABLE
// ASSIGNING) and a binding that fixed it (the row, the object of a
// reference, a typed field symbol) still see it afterwards. A nested
// structure or table is written in place too. src is a fresh value.
export function Overwrite(t, dst, src) {
  if (t.kind === "h") {
    dst.length = 0;
    for (const r of src) dst.push(r);
    return;
  }
  for (const c of t.comps) {
    const k = c.t.kind;
    if ((k === "u" || k === "v" || k === "h") && dst[c.key] !== null && typeof dst[c.key] === "object") Overwrite(c.t, dst[c.key], src[c.key]);
    else dst[c.key] = src[c.key];
  }
}

// dst = src for a generic dst: converted to the type of the slot dst is bound
// to and written there, never rebound. The pairs of go/abap MoveData; any
// other dumps NOT_COMPILED rather than guess.
export function MoveData(dst, src) {
  if (dst === null) throw notAssigned("move into a field symbol");
  if (src === null) throw notAssigned("move from a field symbol");
  const dk = dst.t.kind;
  const sk = src.t.kind;
  const v = src.get();
  switch (dk) {
    case "u": case "v": case "h":
      if (dst.t === src.t) return Overwrite(dst.t, dst.get(), copy(v));
      break;
    case "I":
      if (sk === "I") return dst.set(v);
      if (sk === "F") return dst.set(F2I(v));
      if (sk === "C" || sk === "g") return dst.set(ParseI(v));
      break;
    case "F":
      if (sk === "I" || sk === "F") return dst.set(v);
      if (sk === "C" || sk === "g") return dst.set(ParseF(v));
      break;
    case "8":
      if (sk === "8") return dst.set(v);
      break;
    case "g":
      if (sk === "g" || sk === "C") return dst.set(v);
      if (sk === "I") return dst.set(IToString(v));
      break;
    case "C":
      if (sk === "g" || sk === "C") return dst.set(CFit(v, dst.t.len));
      break;
    case "X":
      if (sk === "X") return dst.set(XFit(v, dst.t.len));
      break;
    case "y": case "D": case "T": case "P":
      if (sk === dk && (dk !== "P" || dst.t === src.t)) return dst.set(v);
      break;
    case "l":
      if (sk === "l") return dst.set(v);
      break;
    default: break;
  }
  throw new AbapError("NOT_COMPILED", `move: a value of type kind ${sk} into generic data of type kind ${dk}`);
}

// CLEAR of a generic value: the slot it is bound to becomes initial
export function ClearData(d) {
  if (d === null) throw notAssigned("CLEAR of a field symbol");
  switch (d.t.kind) {
    case "I": case "F": return d.set(0);
    case "8": return d.set(0n);
    case "g": case "y": case "C": return d.set("");
    case "D": return d.set("00000000");
    case "T": return d.set("000000");
    case "P": return d.set("0");
    case "X": return d.set("\u0000".repeat(d.t.len));
    case "l": return d.set(null);
    case "u": case "v": case "h": return Overwrite(d.t, d.get(), d.t.zero());
    default: throw new AbapError("NOT_COMPILED", `CLEAR: generic data of type kind ${d.t.kind}`);
  }
}

// CALL METHOD (class)=>m, as go/abap CallStatic: the classes the program
// compiled, the ones the registry has (a class that exists but was not
// compiled dumps instead of reading as unknown), and an adapter per static
// method a dynamic call names, reading its arguments out of generic data.
const compiledClasses = new Set();
const registryClasses = new Set();
const statics = new Map();
export function knownClasses(compiled, known) {
  for (const c of compiled) compiledClasses.add(c);
  for (const c of known) registryClasses.add(c);
}
export function registerStatic(name, params, call) { statics.set(name, {params: new Set(params), call}); }
export function CallStatic(s, cls, method, args) {
  const c = String(cls).replace(/ +$/, "");
  if (!compiledClasses.has(c)) {
    if (registryClasses.has(c)) throw new AbapError("NOT_COMPILED", `CALL METHOD (${c})=>${method}: the class exists but is not compiled in this program`);
    throw new AbapError("CX_SY_DYN_CALL_ILLEGAL_CLASS", `CALL METHOD (${c})=>${method}`);
  }
  const e = statics.get(`${c}=>${method}`);
  if (e === undefined) throw new AbapError("CX_SY_DYN_CALL_ILLEGAL_METHOD", `CALL METHOD (${c})=>${method}`);
  for (const n of Object.keys(args)) if (!e.params.has(n)) throw new AbapError("CX_SY_DYN_CALL_PARAM_NOT_FOUND", `${c}=>${method} ${n}`);
  e.call(s, args);
}
export function paramMissing(op) { throw new AbapError("CX_SY_DYN_CALL_PARAM_MISSING", op); }
// d -> i, measured on A4H (go/abap/datesplit.go DToI says how)
export function DToI(v) {
  if (!/^\d{8}$/.test(v)) return 0;
  const y = Number(v.slice(0, 4)), m = Number(v.slice(4, 6)), d = Number(v.slice(6, 8));
  if (y < 1 || m < 1 || m > 12 || d < 1) return 0;
  const key = y * 10000 + m * 100 + d;
  const julian = key < 15821015;
  if (key > 15821004 && julian) throw new AbapError("NOT_COMPILED", "d -> i: a date of the ten days skipped in October 1582 is not measured");
  let leap = y % 4 === 0;
  if (!julian) leap = leap && (y % 100 !== 0 || y % 400 === 0);
  const days = [0, 31, leap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
  if (d > days[m]) return 0;
  const a = Math.trunc((14 - m) / 12), yy = y + 4800 - a, mm = m + 12 * a - 3;
  let jdn = d + Math.trunc((153 * mm + 2) / 5) + 365 * yy + Math.trunc(yy / 4);
  jdn += julian ? -32083 : -Math.trunc(yy / 100) + Math.trunc(yy / 400) - 32045;
  return jdn - 1721424;
}
// SPLIT ... INTO n fields (go/abap/datesplit.go SplitN)
export function SplitN(v, sep, n) {
  if (sep === "") throw new AbapError("NOT_COMPILED", "SPLIT: at an empty separator is not measured");
  const out = new Array(n).fill("");
  const parts = v.split(sep);
  for (let i = 0; i < n && i < parts.length; i += 1) out[i] = i === n - 1 ? parts.slice(i).join(sep) : parts[i];
  return out;
}
export function SplitFit(s, piece, n) {
  if ([...piece.replace(/ +$/, "")].length > n) s.sy.subrc = 4;
  return CFit(piece, n);
}

// IS INITIAL of a d, t or n value: "" (never set) or its typed zero
export const InitialCh = (v, z) => v === "" || v === z;
// IS INITIAL of a structure or table: compared with its initial value
// component by component, a table by its lines
export function IsInitialDeep(v, z) {
  if (Array.isArray(z)) return v.length === 0;
  if (z !== null && typeof z === "object") return Object.keys(z).every((k) => IsInitialDeep(v[k], z[k]));
  if (typeof z === "string") return v === z || v === "";
  return v === z;
}
