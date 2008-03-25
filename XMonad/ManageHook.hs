{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.ManageHook
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
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Display,Window)
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal)

liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged.
idHook :: ManageHook
idHook = doF id

-- | Compose two 'ManageHook's
(<+>) :: ManageHook -> ManageHook -> ManageHook
(<+>) = mappend

-- | Compose the list of 'ManageHook's
composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

-- | 'p --> x'.  If 'p' returns 'True', execute the 'ManageHook'.
(-->) :: Query Bool -> ManageHook -> ManageHook
p --> f = p >>= \b -> if b then f else mempty

-- | 'q =? x'. if the result of 'q' equals 'x', return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | 'p <&&> q'.  '&&' lifted to a Monad.
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- | 'p <||> q'.  '||' lifted to a Monad.
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

-- | Queries that return the window title, resource, or class.
title, resource, className :: Query String
title     = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap (fromMaybe "") $ io $ fetchName    d w)
resource  = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resName        $ io $ getClassHint d w)
className = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resClass       $ io $ getClassHint d w)

-- | A query that can return an arbitrary X property of type String,
--   identified by name.
stringProperty :: String -> Query String
stringProperty p = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap (fromMaybe "") $ getStringProperty d w p)

getStringProperty :: Display -> Window -> String -> X (Maybe String)
getStringProperty d w p = do
  a  <- getAtom p
  md <- io $ getWindowProperty8 d a w
  return $ fmap (map (toEnum . fromIntegral)) md

-- | Modify the 'WindowSet' with a pure function.
doF :: (WindowSet -> WindowSet) -> ManageHook
doF = return . Endo

-- | Move the window to the floating layer.
doFloat :: ManageHook
doFloat = ask >>= \w -> doF . W.float w . snd =<< liftX (floatLocation w)

-- | Map the window and remove it from the 'WindowSet'.
doIgnore :: ManageHook
doIgnore = ask >>= \w -> liftX (reveal w) >> doF (W.delete w)
