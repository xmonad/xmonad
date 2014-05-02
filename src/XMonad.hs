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
    module XMonad.Layout,
    module XMonad.ManageHook,
    module XMonad.Operations,
    module Graphics.X11,
    module Graphics.X11.Xlib.Extras,
    (.|.),
    MonadState(..), gets, modify,
    MonadReader(..), asks,
    MonadIO(..)

 ) where

-- core modules
import XMonad.Main
import XMonad.Core
import XMonad.Config
import XMonad.Layout
import XMonad.ManageHook
import XMonad.Operations
-- import XMonad.StackSet -- conflicts with 'workspaces' defined in XMonad.hs

-- modules needed to get basic configuration working
import Data.Bits
import Graphics.X11 hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras

import Control.Monad.State
import Control.Monad.Reader
