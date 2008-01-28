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
        ["--recompile"]       -> recompile False >> return ()
        ["--recompile-force"] -> recompile True >> return ()
        ["--version"]         -> putStrLn "xmonad 0.5"
#ifdef TESTING
        ("--run-tests":_)     -> Properties.main
#endif
        _                     -> fail "unrecognized flags"

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
