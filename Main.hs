----------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-- xmonad, a minimalist, tiling window manager for X11
--
-----------------------------------------------------------------------------

module Main (main) where

import XMonad

import Control.Monad (unless)
import System.Info
import System.Environment
import System.Posix.Process (executeFile)
import System.Exit (exitFailure)

import Paths_xmonad (version)
import Data.Version (showVersion)

import Graphics.X11.Xinerama (compiledWithXinerama)

#ifdef TESTING
import qualified Properties
#endif

-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
main :: IO ()
main = do
    installSignalHandlers -- important to ignore SIGCHLD to avoid zombies
    args <- getArgs
    let launch = catchIO buildLaunch >> xmonad defaultConfig
    case args of
        []                    -> launch
        ("--resume":_)        -> launch
        ["--help"]            -> usage
        ["--recompile"]       -> recompile True >>= flip unless exitFailure
        ["--replace"]         -> launch
        ["--restart"]         -> sendRestart >> return ()
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion ++ longVersion
#ifdef TESTING
        ("--run-tests":_)     -> Properties.main
#endif
        _                     -> fail "unrecognized flags"
 where
    shortVersion = ["xmonad", showVersion version]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch ++ "-" ++ os
                   , "\nXinerama:", show compiledWithXinerama ]

usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        concat ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help                       Print this message" :
        "  --version                    Print the version number" :
        "  --recompile                  Recompile your ~/.xmonad/xmonad.hs" :
        "  --replace                    Replace the running window manager with xmonad" :
        "  --restart                    Request a running xmonad process to restart" :
#ifdef TESTING
        "  --run-tests                  Run the test suite" :
#endif
        []

-- | Build "~\/.xmonad\/xmonad.hs" with ghc, then execute it.  If there are no
-- errors, this function does not return.  An exception is raised in any of
-- these cases:
--
--   * ghc missing
--
--   * both "~\/.xmonad\/xmonad.hs" and "~\/.xmonad\/xmonad-$arch-$os" missing
--
--   * xmonad.hs fails to compile
--
--      ** wrong ghc in path (fails to compile)
--
--      ** type error, syntax error, ..
--
--   * Missing XMonad\/XMonadContrib modules due to ghc upgrade
--
buildLaunch ::  IO ()
buildLaunch = do
    recompile False
    dir  <- getXMonadDir
    args <- getArgs
    executeFile (dir ++ "/xmonad-"++arch++"-"++os) False args Nothing
    return ()

sendRestart :: IO ()
sendRestart = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw xmonad_restart 32 0 currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False
