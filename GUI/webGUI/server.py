#!/usr/bin/env python3
"""
crackNum web GUI -- the argv adapter.

This is the web analogue of the Swift GUI's runCrackNum() and the Windows GUI's
Runner.cs: it builds an argv, runs the *unmodified* crackNum binary, and returns
its combined stdout+stderr verbatim. It parses none of the output and knows
nothing about floating-point; every answer the browser shows is produced by
crackNum and z3 exactly as they are on the command line.

Stdlib only (Python 3.9+), so it adds no toolchain anywhere it runs.

    ./server.py [--port 8080] [--host 127.0.0.1]

crackNum and z3 are located on PATH, or via $CRACKNUM and $SBV_Z3.
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import sys
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path

STATIC = Path(__file__).resolve().parent / "static"

# How long a single crack may take. crackNum answers in ~40ms; anything near
# this bound means z3 has gone off the rails, and the user gets a clean error
# instead of a hung request.
TIMEOUT_SECS = 10

# The longest value we will hand to crackNum. Generous next to a 64-lane
# Verilog pattern, small enough that nobody can post a novel.
MAX_VALUE_LEN = 512

# Bounds for the custom-width boxes. crackNum itself owns the real limits and
# reports them readably (see Main.hs: z3's FP sort needs >= 2 of each); these
# only stop absurd input from ever reaching it.
MAX_BIT_WIDTH = 4096

ROUNDING_MODES = ["RNE", "RNA", "RTP", "RTN", "RTZ"]
ROUNDING_LABELS = {
    "RNE": "RNE (Nearest, ties to even)",
    "RNA": "RNA (Nearest, ties to away)",
    "RTP": "RTP (Toward +∞)",
    "RTN": "RTN (Toward -∞)",
    "RTZ": "RTZ (Toward 0)",
}

# The sidebar. Mirrors formatSections in GUI/swiftGUI/Sources/CrackNum/CrackNum.swift:
# same ids, same labels, same grouping and order. Served to the browser at
# /api/formats so the table lives in exactly one place.
FORMAT_SECTIONS = [
    ("AI formats", [
        ("ffp4",     "FP4 (E2M1)", "fixedFloat", "-ffp4"),
        ("ffp4e0m3", "FP4 (E0M3)", "fixedFloat", "-ffp4e0m3"),
        ("fe4m3",    "FP8 (E4M3)", "fixedFloat", "-fe4m3"),
        ("fe5m2",    "FP8 (E5M2)", "fixedFloat", "-fe5m2"),
        ("fe8m0",    "FP8 (E8M0)", "fixedFloat", "-fe8m0"),
        ("fbp",      "Brain",      "fixedFloat", "-fbp"),
        ("ftf32",    "TF32",       "fixedFloat", "-ftf32"),
    ]),
    ("IEEE-754", [
        ("fhp", "Half",   "fixedFloat",  "-fhp"),
        ("fsp", "Single", "fixedFloat",  "-fsp"),
        ("fdp", "Double", "fixedFloat",  "-fdp"),
        ("fqp", "Quad",   "fixedFloat",  "-fqp"),
        ("fcs", "Custom", "customFloat", None),
    ]),
    ("Integer (Signed)", [
        ("i8",  "8-bit",  "fixedInt",  "-i8"),
        ("i16", "16-bit", "fixedInt",  "-i16"),
        ("i32", "32-bit", "fixedInt",  "-i32"),
        ("i64", "64-bit", "fixedInt",  "-i64"),
        ("ics", "Custom", "customInt", None),
    ]),
    ("Word (Unsigned)", [
        ("w8",  "8-bit",  "fixedWord",  "-w8"),
        ("w16", "16-bit", "fixedWord",  "-w16"),
        ("w32", "32-bit", "fixedWord",  "-w32"),
        ("w64", "64-bit", "fixedWord",  "-w64"),
        ("wcs", "Custom", "customWord", None),
    ]),
]

FORMATS = {}
for _title, _entries in FORMAT_SECTIONS:
    for _id, _label, _kind, _flag in _entries:
        FORMATS[_id] = {"id": _id, "label": _label, "kind": _kind, "flag": _flag}


def formats_payload():
    return {
        "sections": [
            {"title": t,
             "formats": [{"id": i, "label": l, "kind": k} for (i, l, k, _f) in e]}
            for (t, e) in FORMAT_SECTIONS
        ],
        "roundingModes": [{"id": r, "label": ROUNDING_LABELS[r]} for r in ROUNDING_MODES],
    }


class Missing(Exception):
    """A tool we need is not on PATH."""


def locate(name, env_var):
    """Find a binary: $ENV_VAR first, then PATH. Insist on a real file --
    os.access is true for directories too, which is how a directory named
    crackNum on the PATH broke the Swift GUI's tool discovery (fixed in 3.20)."""
    override = os.environ.get(env_var)
    if override:
        p = Path(override)
        if p.is_file() and os.access(str(p), os.X_OK):
            return str(p)
        raise Missing("%s is set to %r, which is not an executable file." % (env_var, override))
    found = shutil.which(name)
    if found and Path(found).is_file():
        return found
    raise Missing(
        "Cannot locate the '%s' binary on the server's PATH.\n\n"
        "Make sure it is installed and reachable (e.g. `which %s` works)." % (name, name)
    )


def precision_flag(fmt, bit_width, exp_width):
    """Build the crackNum precision flag. Mirrors precisionFlag() in the Swift GUI,
    including its validation message for a malformed custom float."""
    kind = fmt["kind"]
    if kind in ("fixedFloat", "fixedInt", "fixedWord"):
        return fmt["flag"], None
    if kind == "customFloat":
        sig_width = bit_width - exp_width - 1
        if exp_width < 1 or sig_width < 0:
            msg = (
                "Invalid custom FP format:\n"
                "  Total width: %d\n"
                "    Sign       :    1\n"
                "    Exponent   : %4d\n"
                "    Significand: %4d (Total = Sign + Exponent + Significand)\n\n"
                "Exponent must be at least 1 bit, and the total width must leave "
                "room for it and the sign." % (bit_width, exp_width, sig_width)
            )
            return None, msg
        # crackNum's -fE+S: E exponent bits, S significand bits *including* the implied bit.
        return "-f%d+%d" % (exp_width, bit_width - exp_width), None
    if kind == "customInt":
        return "-i%d" % bit_width, None
    if kind == "customWord":
        return "-w%d" % bit_width, None
    return None, "Unknown format kind: %s" % kind


# A contact address for the "Bugs/Feedback/Comments?" link, supplied by the
# deployment rather than the source: this repository is public and an address
# committed to it is an address harvested from it. Unset -- as in any fresh
# checkout -- simply means no link is shown.
#
#     CRACKNUM_CONTACT=you@example.com ./server.py
#
CONTACT_ENV = "CRACKNUM_CONTACT"

# Conservative: what may appear in a mailto: we are about to put in the page.
# Rejecting rather than escaping keeps a malformed value from becoming a
# question about how well the front end escapes.
CONTACT_OK = re.compile(r"^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$")


def contact_address(warn=False):
    """The configured contact address, or None. Never sourced from the repo.

    `warn` only at startup: a bad value should be said once, not once per
    request, or the complaint becomes part of the noise it is trying to escape."""
    raw = (os.environ.get(CONTACT_ENV) or "").strip()
    if not raw:
        return None
    if not CONTACT_OK.match(raw):
        if warn:
            sys.stderr.write("WARNING: %s=%r is not a plausible email address; "
                             "the feedback link will be omitted.\n" % (CONTACT_ENV, raw))
        return None
    return raw


def cracknum_version():
    """Ask the binary what version it is. Parsing our own -v output is still
    better than a second copy of the number here, which would drift the first
    time a release bumped the cabal file and not this file."""
    try:
        cracknum = locate("crackNum", "CRACKNUM")
    except Missing:
        return None
    try:
        proc = subprocess.run(
            [cracknum, "-v"],
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,
            stdin=subprocess.DEVNULL,
            timeout=TIMEOUT_SECS,
        )
    except (subprocess.TimeoutExpired, OSError):
        return None
    if proc.returncode != 0:
        return None
    # "crackNum v4.3, (c) Levent Erkok. Released with a BSD3 license."
    m = re.search(r"\bv(\d[\w.]*)", proc.stdout.decode("utf-8", "replace"))
    return m.group(1) if m else None


def meta_payload():
    """Small footer facts. Computed per request: cheap next to a crack, and it
    means a binary swapped underneath a running server is reported honestly."""
    return {"version": cracknum_version(), "contact": contact_address()}


def run_cracknum(flag, rounding, value):
    """Run the real binary and return its combined output. No shell, ever: argv is
    a list, and the value goes after '--' as a single element so a leading '-'
    cannot be read as a flag."""
    try:
        cracknum = locate("crackNum", "CRACKNUM")
        z3 = locate("z3", "SBV_Z3")
    except Missing as e:
        return str(e)

    # We never pass -l: crackNum infers the lane count from Verilog (N'h) input,
    # and everything else is a single lane. Same choice the Swift GUI makes.
    args = [flag, "-r" + rounding, "--", value]

    env = dict(os.environ)
    env["SBV_Z3"] = z3
    env["PATH"] = os.pathsep.join([str(Path(z3).parent), str(Path(cracknum).parent), env.get("PATH", "")])

    try:
        proc = subprocess.run(
            [cracknum] + args,
            stdout=subprocess.PIPE,
            stderr=subprocess.STDOUT,   # merge, as the Swift and Tcl GUIs do
            stdin=subprocess.DEVNULL,   # crackNum reads its value from argv, not stdin
            env=env,
            timeout=TIMEOUT_SECS,
        )
    except subprocess.TimeoutExpired:
        return ("crackNum timed out after %d seconds.\n\n"
                "That should never happen for a well-formed input -- please report it." % TIMEOUT_SECS)
    except OSError as e:
        return "Failed to launch crackNum: %s" % e

    text = proc.stdout.decode("utf-8", "replace")
    if proc.returncode != 0:
        # Show the argv, but not the server's filesystem layout.
        text += "\n\n** Call to crackNum failed! Make sure the value makes sense for the chosen format."
        text += "\n**"
        text += "\n**   Run: crackNum " + " ".join(args)
        text += "\n**"
        text += "\n**   Value : " + value
    return text


def crack(req):
    """Validate a request and produce the output pane's text."""
    fmt_id = req.get("format")
    fmt = FORMATS.get(fmt_id) if isinstance(fmt_id, str) else None
    if fmt is None:
        return {"text": "Pick a format on the left to crack a value."}

    rounding = req.get("rounding", "RNE")
    if rounding not in ROUNDING_MODES:
        rounding = "RNE"

    value = req.get("value", "")
    if not isinstance(value, str):
        value = ""
    if len(value) > MAX_VALUE_LEN:
        return {"text": "Value is too long (limit %d characters)." % MAX_VALUE_LEN}

    def as_width(key, dflt):
        v = req.get(key, dflt)
        try:
            n = int(v)
        except (TypeError, ValueError):
            return None
        return n if 0 <= n <= MAX_BIT_WIDTH else None

    bit_width = as_width("width", 64)
    exp_width = as_width("exp", 11)
    if bit_width is None or exp_width is None:
        return {"text": "Widths must be whole numbers between 0 and %d." % MAX_BIT_WIDTH}

    flag, invalid = precision_flag(fmt, bit_width, exp_width)
    if invalid is not None:
        return {"text": invalid}

    # An empty box is not a value. This used to default to "0" in the Swift GUI,
    # which cracked a number the user had never typed and presented it exactly
    # like a real result -- right down to 'Conversion from "0" was exact'.
    if not value.strip():
        return {"text": "Enter a value above to crack it."}

    text = run_cracknum(flag, rounding, value)

    if "ENCODED" in text:
        kind = "Encoding in format"
    elif "DECODED" in text:
        kind = "Decoded using format"
    else:
        kind = "Format"
    return {"text": "[%s: %s]\n\n%s" % (kind, fmt["label"], text)}


MIME = {
    ".html": "text/html; charset=utf-8",
    ".css": "text/css; charset=utf-8",
    ".js": "text/javascript; charset=utf-8",
    ".svg": "image/svg+xml",
    ".png": "image/png",
    ".ico": "image/x-icon",
}


class Handler(BaseHTTPRequestHandler):
    server_version = "crackNumWebGUI"
    protocol_version = "HTTP/1.1"

    def log_message(self, fmt, *args):
        sys.stderr.write("%s - %s\n" % (self.address_string(), fmt % args))

    def _send(self, code, body, ctype):
        if isinstance(body, str):
            body = body.encode("utf-8")
        self.send_response(code)
        self.send_header("Content-Type", ctype)
        self.send_header("Content-Length", str(len(body)))
        self.send_header("X-Content-Type-Options", "nosniff")
        self.end_headers()
        if self.command != "HEAD":
            self.wfile.write(body)

    def _json(self, code, obj):
        self._send(code, json.dumps(obj), "application/json; charset=utf-8")

    def do_GET(self):
        path = self.path.split("?", 1)[0].split("#", 1)[0]
        if path == "/api/formats":
            return self._json(200, formats_payload())
        if path == "/api/meta":
            return self._json(200, meta_payload())
        if path == "/healthz":
            return self._send(200, "ok\n", "text/plain; charset=utf-8")

        name = "index.html" if path in ("/", "") else path.lstrip("/")
        # Resolve inside STATIC and refuse anything that escapes it.
        target = (STATIC / name).resolve()
        try:
            target.relative_to(STATIC)
        except ValueError:
            return self._send(403, "Forbidden\n", "text/plain; charset=utf-8")
        if not target.is_file():
            return self._send(404, "Not found\n", "text/plain; charset=utf-8")
        return self._send(200, target.read_bytes(), MIME.get(target.suffix, "application/octet-stream"))

    do_HEAD = do_GET

    def do_POST(self):
        path = self.path.split("?", 1)[0]
        if path != "/api/crack":
            return self._send(404, "Not found\n", "text/plain; charset=utf-8")
        try:
            length = int(self.headers.get("Content-Length", "0"))
        except ValueError:
            length = 0
        if length <= 0 or length > 64 * 1024:
            return self._json(400, {"text": "Bad request."})
        try:
            req = json.loads(self.rfile.read(length).decode("utf-8"))
        except (ValueError, UnicodeDecodeError):
            return self._json(400, {"text": "Bad request."})
        if not isinstance(req, dict):
            return self._json(400, {"text": "Bad request."})
        return self._json(200, crack(req))


def main():
    ap = argparse.ArgumentParser(description="crackNum web GUI")
    ap.add_argument("--port", type=int, default=8080)
    ap.add_argument("--host", default="127.0.0.1",
                    help="bind address; defaults to loopback. Use 0.0.0.0 to serve a network.")
    opts = ap.parse_args()

    for name, var in (("crackNum", "CRACKNUM"), ("z3", "SBV_Z3")):
        try:
            sys.stderr.write("Using %-8s %s\n" % (name + ":", locate(name, var)))
        except Missing as e:
            sys.stderr.write("WARNING: %s\n" % str(e).splitlines()[0])

    contact = contact_address(warn=True)
    sys.stderr.write("Using %-8s %s\n" % ("contact:", contact if contact
                                          else "(none; set %s to show a feedback link)" % CONTACT_ENV))

    srv = ThreadingHTTPServer((opts.host, opts.port), Handler)
    sys.stderr.write("crackNum web GUI on http://%s:%d/\n" % (opts.host, opts.port))
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        sys.stderr.write("\nBye.\n")


if __name__ == "__main__":
    main()
