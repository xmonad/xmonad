#!/usr/bin/env runhaskell

import System.Cmd

-- generate appropriate .hpc files
main = do
    system $ "rm -rf *.tix"
    system $ "dist/build/xmonad/xmonad --run-tests"
    system $ "hpc markup xmonad --exclude=Main --exclude=Properties --exclude=XMonad --exclude=Paths_xmonad"
    system $ "hpc report xmonad --exclude=Main --exclude=Properties --exclude=XMonad --exclude=Paths_xmonad"
