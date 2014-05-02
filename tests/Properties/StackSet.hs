{-# LANGUAGE ScopedTypeVariables #-}
module Properties.StackSet where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.Maybe

import Data.List (nub)
-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet

-- Some general hints for creating StackSet properties:
--
-- *  ops that mutate the StackSet are usually local
-- *  most ops on StackSet should either be trivially reversible, or
--    idempotent, or both.
------------------------------------------------------------------------

-- Basic data invariants of the StackSet
--
-- With the new zipper-based StackSet, tracking focus is no longer an
-- issue: the data structure enforces focus by construction.
--
-- But we still need to ensure there are no duplicates, and master/and
-- the xinerama mapping aren't checked by the data structure at all.
--
-- * no element should ever appear more than once in a StackSet
-- * the xinerama screen map should be:
--          -- keys should always index valid workspaces
--          -- monotonically ascending in the elements
-- * the current workspace should be a member of the xinerama screens
--
invariant (s :: T) = and
    -- no duplicates
    [ noDuplicates

    -- TODO: Fix this.
    -- all this xinerama stuff says we don't have the right structure
--  , validScreens
--  , validWorkspaces
--  , inBounds
    ]
  where
    ws = concat [ focus t : up t ++ down t
                  | w <- workspace (current s) : map workspace (visible s) ++ hidden s
                  , t <- maybeToList (stack w)] :: [Char]
    noDuplicates = nub ws == ws

--  validScreens = monotonic . sort . M. . (W.current s : W.visible : W$ s

--  validWorkspaces = and [ w `elem` allworkspaces | w <- (M.keys . screens) s ]
--          where allworkspaces = map tag $ current s : prev s ++ next s

--  inBounds  = and [ w >=0 && w < size s | (w,sc) <- M.assocs (screens s) ]

monotonic []       = True
monotonic (x:[])   = True
monotonic (x:y:zs) | x == y-1  = monotonic (y:zs)
                   | otherwise = False

prop_invariant = invariant

-- and check other ops preserve invariants
prop_empty_I  (SizedPositive n) l = forAll (choose (1, fromIntegral n)) $  \m ->
                                      forAll (vector m) $ \ms ->
        invariant $ new l [0..fromIntegral n-1] ms

prop_view_I n (x :: T) =
    invariant $ view n x

prop_greedyView_I n (x :: T) =
    invariant $ greedyView n x

prop_focusUp_I (SizedPositive n) (x :: T) =
    invariant $ applyN (Just n) focusUp x
prop_focusMaster_I (SizedPositive n) (x :: T) =
    invariant $ applyN (Just n) focusMaster x
prop_focusDown_I (SizedPositive n) (x :: T) =
    invariant $ applyN (Just n) focusDown x

prop_focus_I (SizedPositive n) (x :: T) =
    case peek x of
        Nothing -> True
        Just _  -> let w = focus . fromJust . stack . workspace . current $
                           applyN (Just n) focusUp x
                   in invariant $ focusWindow w x

prop_insertUp_I n (x :: T) = invariant $ insertUp n x

prop_delete_I (x :: T) = invariant $
    case peek x of
        Nothing -> x
        Just i  -> delete i x

prop_swap_master_I (x :: T) = invariant $ swapMaster x

prop_swap_left_I  (SizedPositive n) (x :: T) =
    invariant $ applyN (Just n) swapUp x
prop_swap_right_I (SizedPositive n) (x :: T) =
    invariant $ applyN (Just n) swapDown x

prop_shift_I (x :: T) = do
  n <- arbitraryTag x
  return $ invariant $ shift (fromIntegral n) x

prop_shift_win_I (nex :: NonEmptyWindowsStackSet) = do
  let NonEmptyWindowsStackSet x = nex
  w <- arbitraryWindow nex
  n <- arbitraryTag x
  return $ invariant $ shiftWin n w x


-- ---------------------------------------------------------------------


-- empty StackSets have no windows in them
prop_empty (EmptyStackSet x) =
        all (== Nothing) [ stack w | w <- workspace (current x)
                                        : map workspace (visible x) ++ hidden x ]

-- empty StackSets always have focus on first workspace
prop_empty_current (EmptyStackSet x) = currentTag x == head (tags x)

-- no windows will be a member of an empty workspace
prop_member_empty i (EmptyStackSet x) = member i x == False

-- peek either yields nothing on the Empty workspace, or Just a valid window
prop_member_peek (x :: T) =
    case peek x of
        Nothing -> True {- then we don't know anything -}
        Just i  -> member i x
