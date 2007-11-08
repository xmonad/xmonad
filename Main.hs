----------------------------------------------------------------------------
-- |
-- Module      :  Main.hs
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

import XMonad.Main
import XMonad.Config
import XMonad.Core (recompile)

import Control.Exception (handle)
import System.IO
import System.Directory
import System.Environment
import System.Posix.Process (executeFile)

-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
main :: IO ()
main = do
    handle (hPrint stderr) buildLaunch
    xmonad defaultConfig -- if buildLaunch returns, execute the trusted core

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
    recompile
    dir <- fmap (++ "/.xmonad") getHomeDirectory
    args <- getArgs
    executeFile (dir ++ "/xmonad") False args Nothing
    return ()
