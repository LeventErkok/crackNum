crackNum @VERSION@ -- macOS (Apple Silicon / arm64)

Contents:
  crackNum      the command-line tool
  z3            solver; crackNum shells out to it for EVERY operation
  CrackNum.app  the native GUI

------------------------------------------------------------------
Install
------------------------------------------------------------------
Unzip the bundle anywhere you like, and remove the quarantine on
downloaded artifacts, as Mac will not let you run binaries downloaded
from the internet:

    tar -xzvf crackNum-@VERSION@-macos-arm64.tar.gz
    cd crackNum-@VERSION@-macos-arm64
    xattr -dr com.apple.quarantine crackNum z3 CrackNum.app

Now, put everything in a place accessible to you. Below, we use "~/bin",
but change it as needed:

    mkdir -p ~/bin && cp crackNum z3 ~/bin/
    cp -R CrackNum.app /Applications/
    export PATH=$HOME/bin:$PATH      # put this in your login shell's startup file

Check:

    crackNum -f sp 3.5
    crackNum --gui
    crackNum --gui -fsp 2.5

------------------------------------------------------------------
Notes
------------------------------------------------------------------
  * arm64 only -- will NOT run on an Intel Mac.
  * macOS 13+ required for the GUI.
  * If you already have z3 on your PATH, drop the bundled copy.
