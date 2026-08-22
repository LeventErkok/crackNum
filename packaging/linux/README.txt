crackNum @VERSION@ -- Linux (x86_64)

Contents:
  crackNum      the command-line tool
  z3            solver; crackNum shells out to it for EVERY operation
  crackNum.tcl  the Tcl/Tk GUI script

Both binaries are statically linked, so there is nothing to install and no
libc, distribution, or version requirement: they run as-is on any x86_64
Linux, old or new.

------------------------------------------------------------------
Install
------------------------------------------------------------------
Unzip the bundle anywhere you like:

    tar -xzvf crackNum-@VERSION@-linux-x86_64.tar.gz
    cd crackNum-@VERSION@-linux-x86_64

Now, put ALL THREE files in a place accessible to you. Below, we use
"~/bin", but change it as needed:

    mkdir -p ~/bin && cp crackNum z3 crackNum.tcl ~/bin/
    export PATH=$HOME/bin:$PATH      # put this in your shell's startup file

The GUI is a Tcl/Tk script, so unlike the two binaries it does need
something from your system: 'wish' on your PATH.

    sudo apt install tk           # Debian/Ubuntu
    sudo dnf install tk           # RHEL/Fedora
    nix profile install nixpkgs#tk

Check:

    crackNum -f sp 3.5
    crackNum --gui
    crackNum --gui -fsp 2.5

crackNum finds the GUI by looking for crackNum.tcl on your PATH, which
the copy above took care of. If you keep it elsewhere, point at it
directly instead:

    export CRACKNUM_TCL=/path/to/crackNum.tcl

------------------------------------------------------------------
Notes
------------------------------------------------------------------
  * x86_64 only -- will NOT run on an arm64 (aarch64) machine.
  * If you already have z3 on your PATH, drop the bundled copy.
