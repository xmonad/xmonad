{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.ManageHook
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
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
import Graphics.X11.Xlib (Display, Window, internAtom, wM_NAME)
import Control.Exception (bracket, SomeException(..))
import qualified Control.Exception as E
import Control.Monad.Reader
import Data.Maybe
import Data.Monoid
import qualified XMonad.StackSet as W
import XMonad.Operations (floatLocation, reveal, isFixedSizeOrTransient)

-- | Lift an 'X' action to a 'Query'.
liftX :: X a -> Query a
liftX = Query . lift

-- | The identity hook that returns the WindowSet unchanged.
idHook :: Monoid m => m
idHook = mempty

-- | Infix 'mappend'. Compose two 'ManageHook' from right to left.
(<+>) :: Monoid m => m -> m -> m
(<+>) = mappend

-- | Compose the list of 'ManageHook's.
composeAll :: Monoid m => [m] -> m
composeAll = mconcat

infix 0 -->

-- | @p --> x@.  If @p@ returns 'True', execute the 'ManageHook'.
--
-- > (-->) :: Monoid m => Query Bool -> Query m -> Query m -- a simpler type
(-->) :: (Monad m, Monoid a) => m Bool -> m a -> m a
p --> f = p >>= \b -> if b then f else return mempty

-- | @q =? x@. if the result of @q@ equals @x@, return 'True'.
(=?) :: Eq a => Query a -> a -> Query Bool
q =? x = fmap (== x) q

infixr 3 <&&>, <||>

-- | '&&' lifted to a 'Monad'.
(<&&>) :: Monad m => m Bool -> m Bool -> m Bool
(<&&>) x y = ifM x y (pure False)

-- | '||' lifted to a 'Monad'.
(<||>) :: Monad m => m Bool -> m Bool -> m Bool
(<||>) x y = ifM x (pure True) y

-- | If-then-else lifted to a 'Monad'.
ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM mb t f = mb >>= \b -> if b then t else f

-- | Return the window title.
title :: Query String
title = ask >>= \w -> liftX $ do
    d <- asks display
    let
        getProp =
            (internAtom d "_NET_WM_NAME" False >>= getTextProperty d w)
                `E.catch` \(SomeException _) -> getTextProperty d w wM_NAME
        extract prop = do l <- wcTextPropertyToTextList d prop
                          return $ if null l then "" else head l
    io $ bracket getProp (xFree . tp_value) extract `E.catch` \(SomeException _) -> return ""

-- | Return the application name.
appName :: Query String
appName = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resName $ io $ getClassHint d w)

-- | Backwards compatible alias for 'appName'.
resource :: Query String
resource = appName

-- | Return the resource class.
className :: Query String
className = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap resClass $ io $ getClassHint d w)

-- | A query that can return an arbitrary X property of type 'String',
--   identified by name.
stringProperty :: String -> Query String
stringProperty p = ask >>= (\w -> liftX $ withDisplay $ \d -> fmap (fromMaybe "") $ getStringProperty d w p)

getStringProperty :: Display -> Window -> String -> X (Maybe String)
getStringProperty d w p = do
  a  <- getAtom p
  md <- io $ getWindowProperty8 d a w
  return $ fmap (map (toEnum . fromIntegral)) md

-- | Return whether the window will be a floating window or not
willFloat :: Query Bool
willFloat = ask >>= \w -> liftX $ withDisplay $ \d -> isFixedSizeOrTransient d w

-- | Modify the 'WindowSet' with a pure function.
doF :: (s -> s) -> Query (Endo s)
doF = return . Endo

-- | Move the window to the floating layer.
doFloat :: ManageHook
doFloat = ask >>= \w -> doF . W.float w . snd =<< liftX (floatLocation w)

-- | Map the window and remove it from the 'WindowSet'.
doIgnore :: ManageHook
doIgnore = ask >>= \w -> liftX (reveal w) >> doF (W.delete w)

-- | Move the window to a given workspace
doShift :: WorkspaceId -> ManageHook
doShift i = doF . W.shiftWin i =<< ask
