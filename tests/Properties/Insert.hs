{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Insert where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.List (nub)

-- ---------------------------------------------------------------------
-- 'insert'

-- inserting a item into an empty stackset means that item is now a member
prop_insert_empty i (EmptyStackSet x)= member i (insertUp i x)

-- insert should be idempotent
prop_insert_idem i (x :: T) = insertUp i x == insertUp i (insertUp i x)

-- insert when an item is a member should leave the stackset unchanged
prop_insert_duplicate (nex :: NonEmptyWindowsStackSet) = do
  let NonEmptyWindowsStackSet x = nex
  w <- arbitraryWindow nex
  return $ insertUp w x == x

-- push shouldn't change anything but the current workspace
prop_insert_local (x :: T) = do
  i <- arbitrary `suchThat` \i' -> not $ i' `member` x
  return $ hidden_spaces x == hidden_spaces (insertUp i x)

-- Inserting a (unique) list of items into an empty stackset should
-- result in the last inserted element having focus.
prop_insert_peek (EmptyStackSet x) (NonEmptyNubList is) =
    peek (foldr insertUp x is) == Just (head is)

-- insert >> delete is the identity, when i `notElem` .
-- Except for the 'master', which is reset on insert and delete.
--
prop_insert_delete x = do
  n <- arbitrary `suchThat` \n -> not $ n `member` x
  return $ delete n (insertUp n y) == (y :: T)
    where
        y = swapMaster x -- sets the master window to the current focus.
                         -- otherwise, we don't have a rule for where master goes.

-- inserting n elements increases current stack size by n
prop_size_insert is (EmptyStackSet x) =
        size (foldr insertUp x ws ) ==  (length ws)
  where
    ws   = nub is
    size = length . index
