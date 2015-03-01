{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Focus where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.Maybe (fromJust)

-- ---------------------------------------------------------------------
-- rotating focus
--

-- master/focus
--
-- The tiling order, and master window, of a stack is unaffected by focus changes.
--
prop_focus_left_master (SizedPositive n) (x::T) =
    index (applyN (Just n) focusUp x) == index x
prop_focus_right_master (SizedPositive  n) (x::T) =
    index (applyN (Just n) focusDown x) == index x
prop_focus_master_master (SizedPositive n) (x::T) =
    index (applyN (Just n) focusMaster x) == index x

prop_focusWindow_master (NonNegative n) (x :: T) =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = n `mod` length s
                   in index (focusWindow (s !! i) x) == index x

-- shifting focus is trivially reversible
prop_focus_left  (x :: T) = (focusUp  (focusDown x)) == x
prop_focus_right (x :: T) = (focusDown (focusUp  x)) ==  x

-- focus master is idempotent
prop_focusMaster_idem (x :: T) = focusMaster x == focusMaster (focusMaster x)

-- focusWindow actually leaves the window focused...
prop_focusWindow_works (NonNegative (n :: Int)) (x :: T) =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = fromIntegral n `mod` length s
                   in (focus . fromJust . stack . workspace . current) (focusWindow (s !! i) x) == (s !! i)

-- rotation through the height of a stack gets us back to the start
prop_focus_all_l (x :: T) = (foldr (const focusUp) x [1..n]) == x
  where n = length (index x)
prop_focus_all_r (x :: T) = (foldr (const focusDown) x [1..n]) == x
  where n = length (index x)

-- prop_rotate_all (x :: T) = f (f x) == f x
--     f x' = foldr (\_ y -> rotate GT y) x' [1..n]

-- focus is local to the current workspace
prop_focus_down_local (x :: T) = hidden_spaces (focusDown x) == hidden_spaces x
prop_focus_up_local (x :: T) = hidden_spaces (focusUp x) == hidden_spaces x

prop_focus_master_local (x :: T) = hidden_spaces (focusMaster x) == hidden_spaces x

prop_focusWindow_local (NonNegative (n :: Int)) (x::T ) =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = fromIntegral n `mod` length s
                   in hidden_spaces (focusWindow (s !! i) x) == hidden_spaces x

-- On an invalid window, the stackset is unmodified
prop_focusWindow_identity (x::T ) = do
    n <- arbitrary `suchThat` \n' -> not $ n' `member` x
    return $ focusWindow n x == x
