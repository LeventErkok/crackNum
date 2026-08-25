# CrackNum GUI (native Windows)

A native WinForms front-end for the [`crackNum`](../../) command-line tool: pick a
format, type a value, and see the encoding/decoding in detail. The GUI is just a
front-end: all cracking is done by the `crackNum` binary, which the app calls
under the hood.

## Requirements

- The [.NET SDK](https://dotnet.microsoft.com/download) to build.
- Nothing to install to *run* it: the app targets .NET Framework 4.8, which
  ships as part of Windows 10 and 11.
- `crackNum.exe` and `z3.exe` reachable, either beside the GUI or on your `PATH`.

## Build

The app is not part of the Hackage package, so building it needs a clone:

```
> git clone https://github.com/LeventErkok/crackNum.git
> cd crackNum\GUI\winGUI
> dotnet build -c Release
```

## Install

Put the resulting `CrackNumGUI.exe` next to `crackNum.exe`, or point at it with
the `CRACKNUM_GUI` environment variable:

```
> set CRACKNUM_GUI=C:\path\to\CrackNumGUI.exe
```

`crackNum.exe` looks beside itself first, then consults `CRACKNUM_GUI`, then your
`PATH`.

## Using it

Launch via the `crackNum` binary, which forwards any format/rounding flags and
the value to the app:

```
> crackNum --gui                 -- open the graphical interface
> crackNum --gui -fsp 2.5        -- open with single-precision selected, and 2.5 cracked
> crackNum --gui 0xdeadbeef      -- open pre-filled with a value to decode
```

The prebuilt Windows bundle on the
[Releases page](https://github.com/LeventErkok/crackNum/releases) already
contains `CrackNumGUI.exe`, so there is nothing to build if you install that way.
Those binaries are not code-signed, so the first run may raise a SmartScreen
prompt; choose "More info" then "Run anyway".
