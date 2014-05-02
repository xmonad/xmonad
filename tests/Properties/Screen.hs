{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Screen where

import Test.QuickCheck
import Instances

import XMonad.StackSet hiding (filter)
import XMonad.Operations  (applyResizeIncHint, applyMaxSizeHint )
import Graphics.X11.Xlib.Types (Dimension)

prop_screens (x :: T) = n `elem` screens x
 where
    n = current x

-- screens makes sense
prop_screens_works (x :: T) = screens x == current x : visible x


------------------------------------------------------------------------
-- Aspect ratios

prop_resize_inc (NonZero (NonNegative inc_w),NonZero (NonNegative inc_h))  b@(w,h) =
    w' `mod` inc_w == 0 && h' `mod` inc_h == 0
   where (w',h') = applyResizeIncHint a b
         a = (inc_w,inc_h)

prop_resize_inc_extra ((NonNegative inc_w))  b@(w,h) =
     (w,h) == (w',h')
   where (w',h') = applyResizeIncHint a b
         a = (-inc_w,0::Dimension)-- inc_h)

prop_resize_max (NonZero (NonNegative inc_w),NonZero (NonNegative inc_h))  b@(w,h) =
    w' <= inc_w && h' <= inc_h
   where (w',h') = applyMaxSizeHint a b
         a = (inc_w,inc_h)

prop_resize_max_extra ((NonNegative inc_w))  b@(w,h) =
     (w,h) == (w',h')
   where (w',h') = applyMaxSizeHint a b
         a = (-inc_w,0::Dimension)-- inc_h)
