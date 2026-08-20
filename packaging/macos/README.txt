crackNum @VERSION@ -- macOS (Apple Silicon / arm64)

Contents:
  crackNum      the command-line tool
  z3            solver; crackNum shells out to it for EVERY operation
  CrackNum.app  the native GUI (optional)

------------------------------------------------------------------
1. Command line
------------------------------------------------------------------
Put BOTH binaries somewhere on your PATH, e.g.

    mkdir -p ~/bin && cp crackNum z3 ~/bin/
    export PATH=$HOME/bin:$PATH      # add to ~/.zshrc to make it stick

Check:

    crackNum -f sp 3.5

------------------------------------------------------------------
2. GUI (optional)
------------------------------------------------------------------
    cp -R CrackNum.app /Applications/

Then either double-click it, or run:

    crackNum --gui
    crackNum --gui -fsp 2.5

The app is only a front-end: it calls the crackNum binary, which it
finds by asking your login shell for its PATH. So step 1 must be done
first, and the PATH export must be in ~/.zshrc for the app to see it
when launched from Finder.

(~/Applications works too if you'd rather not touch /Applications.)

------------------------------------------------------------------
3. If macOS blocks it
------------------------------------------------------------------
Downloads via browser/Slack/AirDrop get quarantined. Clear it:

    xattr -dr com.apple.quarantine crackNum z3 CrackNum.app

Transferring with scp avoids this entirely.

These are ad-hoc signed, not notarized. Gatekeeper will not vouch for
them; the xattr step above is what makes them run.

------------------------------------------------------------------
Notes
------------------------------------------------------------------
  * arm64 only -- will NOT run on an Intel Mac.
  * macOS 13+ required for the GUI.
  * If you already have z3 on your PATH, drop the bundled copy.
