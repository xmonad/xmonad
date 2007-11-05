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

import XMonad.Core
import XMonad.Config

import Control.Exception (handle)
import System.IO
import System.Process
import System.Directory
import System.Environment
import System.Exit
import System.Posix.Process (executeFile)

-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
main :: IO ()
main = do
    handle (hPrint stderr) buildLaunch
    -- if buildLaunch returns, execute the trusted core
    makeMain defaultConfig

-- | Build "~/.xmonad/Main.hs" with ghc, then execute it.  If there are no
-- errors, this function does not return.  An exception is raised in any of
-- these cases:
--            * ghc missing
--            * ~/.xmonad/Main.hs missing
--            * Main.hs fails to compile
--            * Missing xmonad/XMonadContrib modules due to ghc upgrade
--
buildLaunch ::  IO ()
buildLaunch = do
    dir <- fmap (++ "/.xmonad") getHomeDirectory
    pid <- runProcess "ghc" ["--make", "Main.hs"] (Just dir)
        Nothing Nothing Nothing Nothing
    ExitSuccess <- waitForProcess pid

    args <- getArgs
    executeFile (dir ++ "/Main") False args Nothing
    return ()
