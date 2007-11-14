--------------------------------------------------------------------
-- |
-- Module    : XMonad
-- Copyright : (c) Don Stewart
-- License   : BSD3
--
-- Maintainer: Don Stewart <dons@galois.com>
-- Stability : provisional
-- Portability:
--
--------------------------------------------------------------------
--
-- Useful exports for configuration files.

module XMonad (

    module XMonad.Main,
    module XMonad.Core,
    module XMonad.Config,
 -- module Graphics.X11.Xlib, -- conflicts with lots of extensions
    (.|.)

 ) where

-- core modules
import XMonad.Main
import XMonad.Core
import XMonad.Config
-- import XMonad.StackSet -- conflicts with 'workspaces' defined in XMonad.hs

-- modules needed to get basic configuration working
import Data.Bits
-- import Graphics.X11.Xlib
