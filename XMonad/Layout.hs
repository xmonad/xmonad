{-# OPTIONS_GHC -fglasgow-exts    #-} -- For deriving Data/Typeable
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- The collection of core layouts.
--
-----------------------------------------------------------------------------

module XMonad.Layout (
    Full(..), Tall(..), Mirror(..),
    Resize(..), IncMasterN(..), Choose, (|||), ChangeLayout(..),
    mirrorRect, splitVertically,
    splitHorizontally, splitHorizontallyBy, splitVerticallyBy,

    tile

  ) where

import XMonad.Core

import Graphics.X11 (Rectangle(..))
import qualified XMonad.StackSet as W
import Control.Arrow ((***), second)
import Control.Monad
import Data.Maybe (fromMaybe)

------------------------------------------------------------------------

-- | Change the size of the master pane.
data Resize     = Shrink | Expand   deriving Typeable

-- | Increase the number of clients in the master pane.
data IncMasterN = IncMasterN !Int    deriving Typeable

instance Message Resize
instance Message IncMasterN

-- | Simple fullscreen mode. Renders the focused window fullscreen.
data Full a = Full deriving (Show, Read)

instance LayoutClass Full a

-- | The builtin tiling mode of xmonad. Supports 'Shrink', 'Expand' and
-- 'IncMasterN'.
data Tall a = Tall !Int !Rational !Rational deriving (Show, Read)
                        -- TODO should be capped [0..1] ..

-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass Tall a where
    pureLayout (Tall nmaster _ frac) r s = zip ws rs
      where ws = W.integrate s
            rs = tile frac r nmaster (length ws)

    pureMessage (Tall nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = Tall nmaster delta (max 0 $ frac-delta)
            resize Expand             = Tall nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = Tall (max 0 (nmaster+d)) delta frac

    description _ = "Tall"

-- | Compute the positions for windows using the default two-pane tiling
-- algorithm.
--
-- The screen is divided into two panes. All clients are
-- then partioned between these two panes. One pane, the master, by
-- convention has the least number of windows in it.
tile
    :: Rational  -- ^ @frac@, what proportion of the screen to devote to the master area
    -> Rectangle -- ^ @r@, the rectangle representing the screen
    -> Int       -- ^ @nmaster@, the number of windows in the master pane
    -> Int       -- ^ @n@, the total number of windows to tile
    -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitVertically nmaster r1 ++ splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

--
-- Divide the screen vertically into n subrectangles
--
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = sh `div` fromIntegral n --hmm, this is a fold or map.

-- Not used in the core, but exported
splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- Divide the screen into two rectangles, using a rational to specify the ratio
splitHorizontallyBy, splitVerticallyBy :: RealFrac r => r -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

-- Not used in the core, but exported
splitVerticallyBy f = (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

------------------------------------------------------------------------

-- | Mirror a layout, compute its 90 degree rotated form.
data Mirror l a = Mirror (l a) deriving (Show, Read)

instance LayoutClass l a => LayoutClass (Mirror l) a where
    runLayout (W.Workspace i (Mirror l) ms) r = (map (second mirrorRect) *** fmap Mirror)
                                                `fmap` runLayout (W.Workspace i l ms) (mirrorRect r)
    handleMessage (Mirror l) = fmap (fmap Mirror) . handleMessage l
    description (Mirror l) = "Mirror "++ description l

-- | Mirror a rectangle.
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = (Rectangle ry rx rh rw)

------------------------------------------------------------------------
-- LayoutClass selection manager
-- Layouts that transition between other layouts

-- | Messages to change the current layout.
data ChangeLayout = FirstLayout | NextLayout deriving (Eq, Show, Typeable)

instance Message ChangeLayout

-- | The layout choice combinator
(|||) :: (LayoutClass l a, LayoutClass r a) => l a -> r a -> Choose l r a
(|||) = flip SLeft
infixr 5 |||

-- | A layout that allows users to switch between various layout options.
data Choose l r a = SLeft  (r a) (l a)
                  | SRight (l a) (r a) deriving (Read, Show)

data NextNoWrap = NextNoWrap deriving (Eq, Show, Typeable)
instance Message NextNoWrap

-- This has lots of pseudo duplicated code, we must find a better way
instance (LayoutClass l a, LayoutClass r a) => LayoutClass (Choose l r) a where
    runLayout (W.Workspace i (SLeft  r l) ms) =
        fmap (second . fmap $ SLeft  r) . runLayout (W.Workspace i l ms)
    runLayout (W.Workspace i (SRight l r) ms) =
        fmap (second . fmap $ SRight l) . runLayout (W.Workspace i r ms)

    description (SLeft _ l)  = description l
    description (SRight _ r) = description r

    handleMessage lr m | Just FirstLayout <- fromMessage m = case lr of
        SLeft {}   -> return Nothing
        SRight l r -> fmap (Just . flip SLeft l . fromMaybe r)
                        $ handleMessage r (SomeMessage Hide)

    handleMessage lr m | Just NextLayout <- fromMessage m = do
        mlr <- handleMessage lr $ SomeMessage NextNoWrap
        maybe (handleMessage lr $ SomeMessage FirstLayout) (return . Just) mlr

    handleMessage (SLeft r l) m | Just NextNoWrap <- fromMessage m = do
        handleMessage l (SomeMessage Hide)
        mr <- handleMessage r (SomeMessage FirstLayout)
        return . Just . SRight l $ fromMaybe r mr

    handleMessage lr m | Just ReleaseResources <- fromMessage m =
        liftM2 ((Just .) . cons)
                    (fmap (fromMaybe l) $ handleMessage l m)
                    (fmap (fromMaybe r) $ handleMessage r m)
     where (cons, l, r) = case lr of
                            (SLeft  r' l') -> (flip SLeft, l', r')
                            (SRight l' r') -> (SRight, l', r')

    -- The default cases for left and right:
    handleMessage (SLeft  r l) m = fmap (fmap $ SLeft  r) $ handleMessage l m
    handleMessage (SRight l r) m = fmap (fmap $ SRight l) $ handleMessage r m
