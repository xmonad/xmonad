-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad/ManageHook.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-- An EDSL for ManageHooks
--
-----------------------------------------------------------------------------

-- XXX examples required

module XMonad.ManageHook where

import XMonad.Core
import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Monad
import Data.Maybe
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal)

type ManageHook = Query (WindowSet -> WindowSet)
type Query a    = Window -> X a

-- | The identity hook that returns the WindowSet unchanged.
idHook :: ManageHook
idHook = doF id

-- | Compose two 'ManageHook's
(<+>) :: ManageHook -> ManageHook -> ManageHook
f <+> g = \w -> liftM2 (.) (f w) (g w)

-- | Compose the list of 'ManageHook's
composeAll :: [ManageHook] -> ManageHook
composeAll = foldr (<+>) idHook

-- | 'p --> x'.  If 'p' returns 'True', execute the 'ManageHook'.
(-->) :: Query Bool -> ManageHook -> ManageHook
p --> f = \w -> p w >>= \b -> if b then f w else idHook w

-- | 'q =? x'. if the result of 'q' equals 'x', return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = \w -> fmap (== x) (q w)

-- | Queries that return the window title, resource, or class.
title, resource, className :: Query String
title     = \w -> withDisplay $ \d -> fmap (fromMaybe "") $ io $ fetchName d w
resource  = \w -> withDisplay $ \d -> fmap resName $ io $ getClassHint d w
className = \w -> withDisplay $ \d -> fmap resClass $ io $ getClassHint d w

-- | Modify the 'WindowSet' with a pure function.
doF :: (WindowSet -> WindowSet) -> ManageHook
doF f = const (return f)

-- | Move the window to the floating layer.
doFloat :: ManageHook
doFloat = \w -> fmap (W.float w . snd) (floatLocation w)

-- | Map the window and remove it from the 'WindowSet'.
doIgnore :: ManageHook
doIgnore = \w -> reveal w >> return (W.delete w)
