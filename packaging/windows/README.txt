crackNum @VERSION@ -- Windows (x86_64)

Contents:
  crackNum.exe     the command-line tool
  CrackNumGUI.exe  the graphical interface
  z3.exe           solver; crackNum shells out to it for EVERY operation
  *.dll            z3's Microsoft C runtime, shipped by its authors

There is nothing to install. The GUI targets .NET Framework 4.8, which is
part of Windows 10 and 11, and z3 brings its own C runtime, so no .NET
download and no Visual C++ redistributable is needed.

------------------------------------------------------------------
Install
------------------------------------------------------------------
Unzip the bundle anywhere you like, then keep ALL the files together in
one folder -- crackNum.exe finds the GUI and the solver by looking beside
itself first. Below we use "%USERPROFILE%\bin", but change it as needed:

    mkdir "%USERPROFILE%\bin"
    copy crackNum-@VERSION@-windows-x86_64\* "%USERPROFILE%\bin"

Add that folder to your PATH so you can run it from anywhere. In
PowerShell, to set it permanently for your account:

    [Environment]::SetEnvironmentVariable(
        'PATH', "$env:USERPROFILE\bin;" + [Environment]::GetEnvironmentVariable('PATH','User'), 'User')

Then open a NEW terminal (PATH changes do not reach ones already running).

Check:

    crackNum -f sp 3.5
    crackNum --gui
    crackNum --gui -fsp 2.5

The first time you run either executable, Windows may show a blue
"Windows protected your PC" box. That is SmartScreen reporting that these
binaries are not code-signed, not that anything is wrong with them.
Choose "More info", then "Run anyway". You can avoid the prompt entirely
by right-clicking the .zip BEFORE extracting it, choosing Properties, and
ticking "Unblock".

If you keep the GUI somewhere other than beside crackNum.exe, point at it
directly instead:

    set CRACKNUM_GUI=C:\path\to\CrackNumGUI.exe

------------------------------------------------------------------
Notes
------------------------------------------------------------------
  * x86_64 only -- will NOT run on an arm64 (Windows on ARM) machine.
  * If you already have z3 on your PATH, drop the bundled copy and its
    DLLs. crackNum looks beside itself first, then on your PATH.
  * `crackNum --runTests` additionally needs a `diff` program on your
    PATH; Git for Windows ships one in its usr\bin directory.
