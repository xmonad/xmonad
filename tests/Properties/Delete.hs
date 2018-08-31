{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Delete where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

-- ---------------------------------------------------------------------
-- 'delete'

-- deleting the current item removes it.
prop_delete x =
    case peek x of
        Nothing -> True
        Just i  -> not (member i (delete i x))
    where _ = x :: T

-- delete is reversible with 'insert'.
-- It is the identity, except for the 'master', which is reset on insert and delete.
--
prop_delete_insert (x :: T) =
    case peek x of
        Nothing -> True
        Just n  -> insertUp n (delete n y) == y
    where
        y = swapMaster x

-- delete should be local
prop_delete_local (x :: T) =
    case peek x of
        Nothing -> True
        Just i  -> hidden_spaces x == hidden_spaces (delete i x)

-- delete should not affect focus unless the focused element is what is being deleted
prop_delete_focus = do
  -- There should be at least two windows. One in focus, and some to try and
  -- delete (doesn't have to be windows on the current workspace).  We generate
  -- our own, since we can't rely on NonEmptyWindowsStackSet returning one in
  -- the argument with at least two windows.
  x <- arbitrary `suchThat` \x' -> length (allWindows x') >= 2
  w <- arbitraryWindow (NonEmptyWindowsStackSet x)
       -- Make sure we pick a window that is NOT the currently focused
       `suchThat` \w' -> Just w' /= peek x
  return $ peek (delete w x) == peek x

-- focus movement in the presence of delete:
-- when the last window in the stack set is focused, focus moves `up'.
-- usual case is that it moves 'down'.
prop_delete_focus_end = do
    -- Generate a StackSet with at least two windows on the current workspace.
    x <- arbitrary `suchThat` \(x' :: T) -> length (index x') >= 2
    let w = last (index x)
        y = focusWindow w x -- focus last window in stack
    return $ peek (delete w y) == peek (focusUp y)


-- focus movement in the presence of delete:
-- when not in the last item in the stack, focus moves down
prop_delete_focus_not_end = do
  x <- arbitrary
       -- There must be at least two windows and the current focused is not the
       -- last one in the stack.
       `suchThat` \(x' :: T) ->
         let currWins = index x'
         in length (currWins) >= 2 && peek x' /= Just (last currWins)
  -- This is safe, as we know there are >= 2 windows
  let Just n = peek x
  return $ peek (delete n x) == peek (focusDown x)
