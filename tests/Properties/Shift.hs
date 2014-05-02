{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Shift where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import qualified Data.List as L

-- ---------------------------------------------------------------------
-- shift

-- shift is fully reversible on current window, when focus and master
-- are the same. otherwise, master may move.
prop_shift_reversible (x :: T) = do
    i <- arbitraryTag x
    case peek y of
      Nothing -> return True
      Just _  -> return $ normal ((view n . shift n . view i . shift i) y) == normal y
    where
        y = swapMaster x
        n = currentTag y

------------------------------------------------------------------------
-- shiftMaster

-- focus/local/idempotent same as swapMaster:
prop_shift_master_focus (x :: T) = peek x == (peek $ shiftMaster x)
prop_shift_master_local (x :: T) = hidden_spaces x == hidden_spaces (shiftMaster x)
prop_shift_master_idempotent (x :: T) = shiftMaster (shiftMaster x) == shiftMaster x
-- ordering is constant modulo the focused window:
prop_shift_master_ordering (x :: T) = case peek x of
    Nothing -> True
    Just m  -> L.delete m (index x) == L.delete m (index $ shiftMaster x)

-- ---------------------------------------------------------------------
-- shiftWin

-- shiftWin on current window is the same as shift
prop_shift_win_focus (x :: T) = do
    n <- arbitraryTag x
    case peek x of
      Nothing -> return True
      Just w  -> return $ shiftWin n w x == shift n x

-- shiftWin on a non-existant window is identity
prop_shift_win_indentity (x :: T) = do
    n <- arbitraryTag x
    w <- arbitrary `suchThat` \w' -> not (w' `member` x)
    return $ shiftWin n w x == x

-- shiftWin leaves the current screen as it is, if neither n is the tag
-- of the current workspace nor w on the current workspace
prop_shift_win_fix_current = do
  x <- arbitrary `suchThat` \(x' :: T) ->
         -- Invariant, otherWindows are NOT in the current workspace.
         let otherWindows = allWindows x' L.\\ index x'
         in  length(tags x') >= 2 && length(otherWindows) >= 1
  -- Sadly we have to construct `otherWindows` again, for the actual StackSet
  -- that got chosen.
  let otherWindows = allWindows x L.\\ index x
  -- We know such tag must exists, due to the precondition
  n <- arbitraryTag x `suchThat` (/= currentTag x)
  -- we know length is >= 1, from above precondition
  idx <- choose(0, length(otherWindows) - 1)
  let w = otherWindows !! idx
  return $ (current $ x) == (current $ shiftWin n w x)

