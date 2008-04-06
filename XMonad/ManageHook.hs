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

import Prelude hiding (catch)
import XMonad.Core
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xlib (Display, Window, internAtom, wM_NAME)
import Control.Exception (bracket, catch)
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal)

-- | Lift an 'X' action to a 'Query'.
liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged.
idHook :: ManageHook
idHook = doF id

-- | Compose two 'ManageHook's.
(<+>) :: ManageHook -> ManageHook -> ManageHook
(<+>) = mappend

-- | Compose the list of 'ManageHook's.
composeAll :: [ManageHook] -> ManageHook
composeAll = mconcat

-- | @p --> x@.  If @p@ returns 'True', execute the 'ManageHook'.
(-->) :: Query Bool -> ManageHook -> ManageHook
p --> f = p >>= \b -> if b then f else mempty

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to a Monad.
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) = liftM2 (&&)

-- | '||' lifted to a Monad.
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) = liftM2 (||)

-- | Return the window title.
title :: Query String
title = ask >>= \w -> liftX $ do
    d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `catch` \_ -> getTextProperty d w wM_NAME
        extract = fmap head . wcTextPropertyToTextList d
    io $ bracket getProp (xFree . tp_value) extract `catch` \_ -> return ""

-- | Return the application name.
appName :: Query String
appName = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resName $ io $ getClassHint d w)

-- | Backwards compatible alias for 'appName'.
resource :: Query String
resource = appName

-- | Return the resource class.
className :: Query String
className = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resClass $ io $ getClassHint d w)

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
