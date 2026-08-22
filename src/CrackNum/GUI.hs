---------------------------------------------------------------------------
-- |
-- Module      :  CrackNum.GUI
-- Copyright   :  (c) Levent Erkok
-- License     :  BSD3
-- Maintainer  :  erkokl@gmail.com
-- Stability   :  experimental
--
-- Locating and launching the graphical interface
-----------------------------------------------------------------------------

{-# OPTIONS_GHC -Wall -Werror #-}

module CrackNum.GUI(
     launchGUI
   ) where

import System.Directory   (findExecutable, doesFileExist)
import System.Environment (lookupEnv, getExecutablePath)
import System.Exit        (ExitCode(..))
import System.FilePath    (takeDirectory, (</>))
import System.Process     (rawSystem)
import qualified System.Info as Info

import Paths_crackNum (getDataFileName)

import CrackNum.Utils (die)

-- | Where the Tcl/Tk GUI script lives relative to the package root; also its
-- location within the installed data-directory. (See Data-files in the cabal file.)
tclRelPath :: FilePath
tclRelPath = "GUI/tclGUI/crackNum.tcl"

-- | Locate the Tcl/Tk GUI script. Normally it is installed together with the
-- binary, so this just works; we look in four places, in order:
--
--   1. $CRACKNUM_TCL, if set: an explicit override, mirroring $CRACKNUM_GUI on macOS.
--   2. The PATH, so a source checkout can shadow the installed copy while hacking.
--   3. Next to the executable itself. This is what makes a relocatable binary
--      distribution work: in one the data-directory below was baked in on the
--      build machine, and names a path that does not exist on the user's.
--   4. The copy cabal installed in our data-directory.
locateTcl :: IO FilePath
locateTcl = do mbEnv <- lookupEnv "CRACKNUM_TCL"
               case mbEnv of
                 Just p  -> do ok <- doesFileExist p
                               if ok
                                  then pure p
                                  else die [ "The CRACKNUM_TCL environment variable is set, but does not name a file:"
                                           , ""
                                           , "    " ++ p
                                           ]
                 Nothing -> do beside    <- besideExe
                               installed <- getDataFileName tclRelPath
                               mbPath    <- findExecutable "crackNum.tcl"
                               case mbPath of
                                 Just p  -> pure p
                                 Nothing -> search [beside, installed] (noTcl beside installed)
  where -- NB. getExecutablePath resolves symlinks, so this finds the script even
        -- when the binary is reached through a link from elsewhere on the PATH.
        besideExe = do exe <- getExecutablePath
                       pure (takeDirectory exe </> "crackNum.tcl")

        search []     onFail = die onFail
        search (c:cs) onFail = do ok <- doesFileExist c
                                  if ok then pure c else search cs onFail

        noTcl beside installed =
             [ "Cannot find the CrackNum GUI script (crackNum.tcl)."
             , ""
             , "Looked in:"
             , "  $CRACKNUM_TCL                 (not set)"
             , "  crackNum.tcl on your PATH     (not found)"
             , "  " ++ beside
             , "  " ++ installed
             , ""
             , "This script is normally installed along with crackNum, so seeing this"
             , "means the installed copy is missing or the binary has been moved."
             , ""
             , "If you have a source checkout, point at it directly:"
             , ""
             , "    export CRACKNUM_TCL=/path/to/crackNum/" ++ tclRelPath
             , ""
             , "Otherwise, get a copy of the sources with either of:"
             , ""
             , "    cabal get crackNum"
             , "    git clone http://github.com/LeventErkok/crackNum.git"
             ]

-- | Launch the graphical interface, forwarding all remaining arguments
-- (format flags, rounding mode, and/or the value to crack) so the GUI can preselect
-- them. The GUI itself calls back into this executable to do the actual cracking.
--
-- On macOS the GUI is a Swift/AppKit app; CRACKNUM_GUI can override the .app bundle
-- location. On Linux the GUI is a Tcl/Tk script; 'wish' is located via PATH, and
-- 'crackNum.tcl' via 'locateTcl'.
launchGUI :: [String] -> IO ()
launchGUI vals
  | Info.os == "darwin"
  = do mbApp <- lookupEnv "CRACKNUM_GUI"
       let args = case mbApp of
                    Just p  -> ["-n", p,                "--args"] ++ vals
                    Nothing -> ["-n", "-a", "CrackNum", "--args"] ++ vals
       ec <- rawSystem "open" args
       case ec of
         ExitSuccess   -> pure ()
         ExitFailure _ -> die [ "Unable to launch the CrackNum GUI application."
                              , ""
                              , "The CrackNum GUI app does not seem to be installed. To install it,"
                              , "get the crackNum sources and build the GUI (macOS 13+, Swift toolchain):"
                              , ""
                              , "    git clone http://github.com/LeventErkok/crackNum.git"
                              , "    cd crackNum/GUI/swiftGUI"
                              , "    make install       # builds and copies CrackNum.app into /Applications"
                              , ""
                              , "Then re-run: crackNum --gui" ++ (if null vals then "" else ' ' : unwords vals)
                              ]
  | Info.os == "linux"
  = do mbWish <- findExecutable "wish"
       wish   <- case mbWish of
                   Just w  -> pure w
                   Nothing -> die [ "Cannot find 'wish' on your PATH."
                                  , "Install Tcl/Tk to get wish, e.g.:"
                                  , ""
                                  , "    nix profile install nixpkgs#tk"
                                  , "    sudo apt install tk       # Debian/Ubuntu"
                                  , "    sudo dnf install tk       # RHEL/Fedora"
                                  ]
       tcl    <- locateTcl
       ec     <- rawSystem wish (tcl : vals)
       case ec of
         ExitSuccess   -> pure ()
         ExitFailure _ -> die [ "Unable to launch the CrackNum GUI."
                              , ""
                              , "Tried: " ++ wish ++ " " ++ tcl
                              ]
  | True
  = die [ "The --gui option is not supported on this platform (" ++ Info.os ++ ")."
        , "Use crackNum directly from the command line."
        ]
