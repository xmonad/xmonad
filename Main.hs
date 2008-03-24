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

import System.IO
import System.Info
import System.Environment
import System.Posix.Process (executeFile)

import Paths_xmonad (version)
import Data.Version (showVersion)

#ifdef TESTING
import qualified Properties
#endif

-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
main :: IO ()
main = do
    args <- getArgs
    let launch = catchIO buildLaunch >> xmonad defaultConfig
    case args of
        []                    -> launch
        ["--resume", _]       -> launch
        ["--help"]            -> usage
        ["--recompile"]       -> recompile True >> return ()
        ["--version"]         -> putStrLn ("xmonad " ++ showVersion version)
#ifdef TESTING
        ("--run-tests":_)     -> Properties.main
#endif
        _                     -> fail "unrecognized flags"

usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        concat ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help                       Print this message" :
        "  --version                    Print the version number" :
        "  --recompile                  Recompile your ~/.xmonad/xmonad.hs" :
#ifdef TESTING
        "  --run-tests                  Run the test suite" :
#endif
        "  --resume STATE               Internal flag, do not use" :
        []

-- | Build "~/.xmonad/xmonad.hs" with ghc, then execute it.  If there are no
-- errors, this function does not return.  An exception is raised in any of
-- these cases:
--   * ghc missing
--   * ~/.xmonad/xmonad.hs missing
--   * xmonad.hs fails to compile
--      ** wrong ghc in path (fails to compile)
--      ** type error, syntax error, ..
--   * Missing xmonad/XMonadContrib modules due to ghc upgrade
--
buildLaunch ::  IO ()
buildLaunch = do
    recompile False
    dir  <- getXMonadDir
    args <- getArgs
    executeFile (dir ++ "/xmonad-"++arch++"-"++os) False args Nothing
    return ()
