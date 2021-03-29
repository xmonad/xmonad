{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Screen where

import Utils
import Test.QuickCheck
import Instances

import Control.Applicative
import XMonad.StackSet hiding (filter)
import XMonad.Operations
import Graphics.X11.Xlib.Types (Dimension)

import Graphics.X11 (Rectangle(Rectangle))
import XMonad.Layout

prop_screens (x :: T) = n `elem` screens x
 where
    n = current x

-- screens makes sense
prop_screens_works (x :: T) = screens x == current x : visible x


------------------------------------------------------------------------
-- Hints

prop_resize_inc (Positive inc_w,Positive inc_h)  b@(w,h) =
    w' `mod` inc_w == 0 && h' `mod` inc_h == 0
   where (w',h') = applyResizeIncHint a b
         a = (inc_w,inc_h)

prop_resize_inc_extra ((NonNegative inc_w))  b@(w,h) =
     (w,h) == (w',h')
   where (w',h') = applyResizeIncHint a b
         a = (-inc_w,0::Dimension)-- inc_h)

prop_resize_max (Positive inc_w,Positive inc_h)  b@(w,h) =
    w' <= inc_w && h' <= inc_h
   where (w',h') = applyMaxSizeHint a b
         a = (inc_w,inc_h)

prop_resize_max_extra ((NonNegative inc_w))  b@(w,h) =
     (w,h) == (w',h')
   where (w',h') = applyMaxSizeHint a b
         a = (-inc_w,0::Dimension)-- inc_h)


prop_aspect_hint_shrink hint (w,h) = case applyAspectHint hint (w,h) of
  (w',h') -> w' <= w && h' <= h


-- applyAspectHint does nothing when the supplied (x,y) fits
-- the desired range
prop_aspect_fits =
    forAll ((,,,) <$> pos <*> pos <*> pos <*> pos) $ \ (x,y,a,b) ->
    let f v = applyAspectHint ((x, y+a), (x+b, y)) v
    in  and [ noOverflows (*) x (y+a), noOverflows (*) (x+b) y ]
            ==> f (x,y) == (x,y)

  where pos = choose (0, 65535)

prop_point_within r@(Rectangle x y w h) =
    forAll ((,) <$>
              choose (0, fromIntegral w - 1) <*>
              choose (0, fromIntegral h - 1)) $
        \(dx,dy) ->
    and [ dx > 0, dy > 0,
         noOverflows (\ a b -> a + abs b) x w,
         noOverflows (\ a b -> a + abs b) y h ]
      ==> pointWithin (x+dx) (y+dy) r

prop_point_within_mirror r (x,y) = pointWithin x y r == pointWithin y x (mirrorRect r)
