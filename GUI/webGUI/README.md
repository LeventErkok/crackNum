# crackNum web GUI

A browser front-end for crackNum, alongside the macOS (Swift), Windows (C#) and
Linux (Tcl/Tk) GUIs. Like those, it builds a command line, runs the **unmodified**
`crackNum` binary, and shows the output verbatim. Nothing here re-implements any
part of crackNum: no float parsing, no formatting, no arithmetic. Every answer on
screen came out of `crackNum` and `z3`.

## Running it

    ./server.py                      # http://127.0.0.1:8080/
    ./server.py --port 8111

`crackNum` and `z3` are found on `PATH`, or via `$CRACKNUM` and `$SBV_Z3`. Python
3.9+ stdlib only -- no dependencies to install anywhere it runs.

## What's here

| File | Role |
|---|---|
| `server.py` | The argv adapter. The web analogue of the Swift GUI's `runCrackNum` and the Windows GUI's `Runner.cs`. |
| `static/index.html` | Layout: value box, format sidebar, output pane. |
| `static/app.js` | Builds the request, renders the reply. Parses nothing. |
| `static/style.css` | Chrome, plus the monospace `<pre>` the bit ruler depends on. |

The format list lives in exactly one place -- `FORMAT_SECTIONS` in `server.py` --
and is served to the browser at `/api/formats`, so the sidebar cannot drift from
what the server will accept.

## Behaviour it inherits from the Swift GUI

  * Same sidebar: same ids, labels, grouping and order as `formatSections`.
  * Same argv: `[flag, -r<mode>, --, value]`, with `-l` never passed (crackNum
    infers lanes from Verilog `N'h` input).
  * Same output framing: `[Encoding in format: ...]` / `[Decoded using format: ...]`.
  * Same custom-width rules, including the validation text for a malformed float.
  * An empty value box says so rather than cracking `0` -- inventing input the
    user never typed, and presenting it like a real result, was a real bug.
  * `stderr` merged into `stdout`; the "call failed" footer on a non-zero exit.

## Web-only additions

  * **Permalinks.** `?f=ftf32&v=123.45&r=RTZ&w=&e=` reproduces a result exactly;
    the URL updates as you work, and *Copy link* puts it on the clipboard.
  * Dark mode via `prefers-color-scheme`; the sidebar collapses on narrow screens.

## Safety notes for anything beyond localhost

  * The value is passed as a single `argv` element after `--`. There is no shell
    anywhere in this server: no `shell=True`, no string interpolation into a
    command. Shell metacharacters in the value reach crackNum as literal text.
  * Format ids are looked up in a fixed table; rounding modes in a fixed list;
    widths must be integers in range; values are capped at 512 characters.
  * Every crack runs with a 10-second timeout.
  * Static files are resolved inside `static/` and anything escaping it is refused.
  * `--host` defaults to loopback. Binding `0.0.0.0` exposes it to the network.

`http.server` is fine for localhost and for a small internal deployment, but it is
not a hardened production server. Behind an SSO-terminating reverse proxy on an
internal host it is reasonable; on the public internet it should sit behind a real
proxy with rate limiting.

## Verification

`server.py` has no opinions to test, so the useful check is differential: run the
same argv through the CLI and through `/api/crack` and compare. A 31-case sweep
(every format, all five rounding modes, hex/binary/Verilog/decimal/hex-float
input, NaN/Inf/-0, saturation and rejection cases) is byte-identical to the CLI.

The pixels are a separate question. Layout bugs do not fail builds -- the Windows
GUI shipped a toolbar rendering one button and a heading clipped to "AI formal"
past a green CI run, a compiler, and 371 golden tests. A headless browser driving
this UI and screenshotting it would close that gap; until then, look at it.
