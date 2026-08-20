crackNum @VERSION@ -- Linux (x86_64)

Contents:
  crackNum      the command-line tool
  z3            solver; crackNum shells out to it for EVERY operation
  crackNum.tcl  the Tcl/Tk GUI script (optional)

Both binaries are statically linked, so there is nothing to install and no
libc, distribution, or version requirement: they run as-is on any x86_64
Linux, old or new.

------------------------------------------------------------------
1. Command line
------------------------------------------------------------------
Put ALL THREE files somewhere on your PATH, e.g.

    mkdir -p ~/bin && cp crackNum z3 crackNum.tcl ~/bin/
    export PATH=$HOME/bin:$PATH      # add to ~/.bashrc to make it stick

Check:

    crackNum -f sp 3.5

------------------------------------------------------------------
2. GUI (optional)
------------------------------------------------------------------
The GUI is a Tcl/Tk script, so unlike the two binaries it does need
something from your system: 'wish' on your PATH.

    sudo apt install tk           # Debian/Ubuntu
    sudo dnf install tk           # RHEL/Fedora
    nix profile install nixpkgs#tk

Then:

    crackNum --gui
    crackNum --gui -fsp 2.5

crackNum finds the script by looking for crackNum.tcl on your PATH,
which step 1 already took care of. If you keep it elsewhere, point at
it directly instead:

    export CRACKNUM_TCL=/path/to/crackNum.tcl

The GUI is only a front-end: it calls the crackNum binary, so step 1
is required either way.

------------------------------------------------------------------
Notes
------------------------------------------------------------------
  * x86_64 only -- will NOT run on an arm64 (aarch64) machine.
  * If you already have z3 on your PATH, drop the bundled copy.
