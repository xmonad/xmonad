{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Layout.Tall where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)
import XMonad.Core
import XMonad.Layout

import Graphics.X11.Xlib.Types (Rectangle(..))

import Data.Maybe
import Data.List (sort)
import Data.Ratio

------------------------------------------------------------------------
-- The Tall layout

-- 1 window should always be tiled fullscreen
prop_tile_fullscreen rect = tile pct rect 1 1 == [rect]
    where pct = 1/2

-- multiple windows
prop_tile_non_overlap rect windows nmaster = noOverlaps (tile pct rect nmaster windows)
  where _ = rect :: Rectangle
        pct = 3 % 100

-- splitting horizontally yields sensible results
prop_split_horizontal (NonNegative n) x =
      (noOverflows (+) (rect_x x) (rect_width x)) ==>
        sum (map rect_width xs) == rect_width x
     &&
        all (== rect_height x) (map rect_height xs)
     &&
        (map rect_x xs) == (sort $ map rect_x xs)

    where
        xs = splitHorizontally n x

-- splitting vertically yields sensible results
prop_split_vertical (r :: Rational) x =
        rect_x x == rect_x a && rect_x x == rect_x b
      &&
        rect_width x == rect_width a && rect_width x == rect_width b
    where
        (a,b) = splitVerticallyBy r x


-- pureLayout works.
prop_purelayout_tall n r1 r2 rect = do
  x <- (arbitrary :: Gen T) `suchThat` (isJust . peek)
  let layout = Tall n r1 r2
      st = fromJust . stack . workspace . current $ x
      ts = pureLayout layout rect st
  return $
        length ts == length (index x)
      &&
        noOverlaps (map snd ts)
      &&
        description layout == "Tall"


-- Test message handling of Tall

-- what happens when we send a Shrink message to Tall
prop_shrink_tall (NonNegative n) (Positive delta) (NonNegative frac) =
        n == n' && delta == delta' -- these state components are unchanged
    && frac' <= frac  && (if frac' < frac then frac' == 0 || frac' == frac - delta
                                          else frac == 0 )
        -- remaining fraction should shrink
    where
         l1                   = Tall n delta frac
         Just l2@(Tall n' delta' frac') = l1 `pureMessage` (SomeMessage Shrink)
        --  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)


-- what happens when we send a Shrink message to Tall
prop_expand_tall (NonNegative n)
                 (Positive delta)
                 (NonNegative n1)
                 (Positive d1) =

       n == n'
    && delta == delta' -- these state components are unchanged
    && frac' >= frac
    && (if frac' > frac
           then frac' == 1 || frac' == frac + delta
           else frac == 1 )

        -- remaining fraction should shrink
    where
         frac                 = min 1 (n1 % d1)
         l1                   = Tall n delta frac
         Just l2@(Tall n' delta' frac') = l1 `pureMessage` (SomeMessage Expand)
        --  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)

-- what happens when we send an IncMaster message to Tall
prop_incmaster_tall (NonNegative n) (Positive delta) (NonNegative frac)
                    (NonNegative k) =
       delta == delta'  && frac == frac' && n' == n + k
    where
         l1                   = Tall n delta frac
         Just l2@(Tall n' delta' frac') = l1 `pureMessage` (SomeMessage (IncMasterN k))
        --  pureMessage :: layout a -> SomeMessage -> Maybe (layout a)



     --   toMessage LT = SomeMessage Shrink
     --   toMessage EQ = SomeMessage Expand
     --   toMessage GT = SomeMessage (IncMasterN 1)


prop_desc_mirror n r1 r2 = description (Mirror $! t) == "Mirror Tall"
    where t = Tall n r1 r2
