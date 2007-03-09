-----------------------------------------------------------------------------
-- |
-- Module      :  StackSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable, needs GHC 6.6
--
-----------------------------------------------------------------------------
--
-- The 'StackSet' data type encodes a set of stacks. A given stack in the
-- set is always current. Elements may appear only once in the entire
-- stack set.
--
-- A StackSet provides a nice datastructure for multiscreen
-- windowmanagers, where each screen has a stack of windows, and a window
-- may be on only 1 screen at any given time.
--

module StackSet (

    StackSet,           -- abstract

    -- * Introduction
    empty,              -- :: Int -> StackSet a
    fromList,           -- :: [[a]] -> StackSet a
    toList,             -- :: StackSet -> [[a]]

    -- * Inspection
    size,               -- :: StackSet -> Int
    member,             -- :: Ord a => a -> StackSet a -> Bool
    peek,               -- :: StackSet a -> Maybe a
    stack,              -- :: StackSet a -> [a]
    cursor,             -- :: StackSet a -> Int
    index,              -- :: StackSet a -> Int -> Maybe [a]

    -- * Modification to the current stack
    push,               -- :: Ord a => a -> StackSet a -> StackSet a
    pop,                -- :: Ord a => StackSet a -> StackSet a
    rotate,             -- :: Ordering -> StackSet a -> StackSet a
    shift,              -- :: Ord a => Int -> StackSet a -> StackSet a

    -- * Modification to arbitrary stacks
    delete,             -- :: Ord a => a -> StackSet a -> StackSet a
    insert,             -- :: Ord a => a -> Int -> StackSet a -> StackSet a

    -- * Changing which stack is 'current'
    view,               -- :: Int -> StackSet a -> StackSet a

  ) where

import Data.Maybe
import qualified Data.Foldable as F
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.Sequence as S

------------------------------------------------------------------------

-- | The StackSet data structure. A table of stacks, with a cursor
data StackSet a =
    StackSet
        { cursor :: {-# UNPACK #-} !Int             -- ^ the currently visible stack
        , size   :: {-# UNPACK #-} !Int             -- ^ size of the stack list
        , stacks :: {-# UNPACK #-} !(S.Seq [a])     -- ^ the separate stacks
        , cache  :: {-# UNPACK #-} !(M.Map a Int)   -- ^ a cache of windows back to their stacks
        } deriving Eq

instance Show a => Show (StackSet a) where show = show . toList

-- Ord a constraint on 'a' as we use it as a key.
--
-- The cache is used to check on insertion that we don't already have
-- this window managed on another stack
--
-- Currently stacks are of a fixed size. There's no firm reason to
-- do this (new empty stacks could be created on the fly).

------------------------------------------------------------------------

-- | Create a new empty stacks of size 'n', indexed from 0. The
-- 0-indexed stack will be current.
empty :: Int -> StackSet a
empty n = StackSet { cursor = 0
                   , size   = n -- constant
                   , stacks = S.fromList (replicate n [])
                   , cache  = M.empty
                   }

-- | True if x is somewhere in the StackSet
member :: Ord a => a -> StackSet a -> Bool
member a w = M.member a (cache w)

------------------------------------------------------------------------

-- | fromList. Build a new StackSet from a list of list of elements
-- If there are duplicates in the list, the last occurence wins.
fromList :: Ord a => (Int,[[a]]) -> StackSet a
fromList (_,[])
    = error "Cannot build a StackSet from an empty list"

fromList (n,xs)
    | n < 0 || n >= length xs
    = error $ "Cursor index is out of range: " ++ show (n, length xs)

fromList (o,xs) = view o $
    foldr (\(i,ys) s ->
        foldr (\a t -> insert a i t) s ys)
            (empty (length xs)) (zip [0..] xs)

-- | toList. Flatten a stackset to a list of lists
toList  :: StackSet a -> (Int,[[a]])
toList x = (cursor x, F.toList (stacks x))

------------------------------------------------------------------------

-- | Push. Insert an element onto the top of the current stack. 
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that
-- stack first.
push :: Ord a => a -> StackSet a -> StackSet a
push k w = insert k (cursor w) w

-- | Pop. Pop the element off the top of the stack and discard it.
-- A new StackSet is returned. If the current stack is empty, the
-- original StackSet is returned unchanged.
pop :: Ord a => StackSet a -> StackSet a
pop w = case peek w of
            Nothing -> w
            Just t  -> delete t w

-- | Extract the element on the top of the current stack. If no such
-- element exists, Nothing is returned.
peek :: StackSet a -> Maybe a
peek = listToMaybe . stack

-- | Index. Extract stack at index 'n'. If the index is invalid,
-- Nothing is returned.
index :: StackSet a -> Int -> Maybe [a]
index w n | n < 0 || n >= size w = Nothing
          | otherwise            = Just (stacks w `S.index` n)

-- | Return the current stack
stack :: StackSet a -> [a]
stack w = case index w (cursor w) of
    Just s  -> s
    Nothing -> error $ "current: no 'current' stack in StackSet: " ++ show (cursor w) -- can't happen

-- | rotate. cycle the current window list up or down.
--
--  rotate EQ   -->  [5,6,7,8,1,2,3,4]
--  rotate GT   -->  [6,7,8,1,2,3,4,5]
--  rotate LT   -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: Ordering -> StackSet a -> StackSet a
rotate o = unsafeModify rot -- safe, since 'rot' is guaranteed to only permute the list
    where
        rot s = take l . drop offset . cycle $ s
           where
              n      = fromEnum o - 1
              l      = length s
              offset = if n < 0 then l + n else n

-- ---------------------------------------------------------------------

-- | view. Set the stack specified by the Int argument as being the
-- current StackSet. If the index is out of range, the original
-- StackSet is returned. StackSet are indexed from 0.
view :: Int -> StackSet a -> StackSet a
view n w | n >= 0 && n < size w = w { cursor = n }
         | otherwise            = w

-- | shift. move the client on top of the current stack to the top of stack 'n'.
-- The new StackSet is returned.
--
-- If the stack to move to is not valid, the original StackSet is returned.
-- If there are no elements in the current stack, nothing changes.
--
shift :: Ord a => Int -> StackSet a -> StackSet a
shift n w | n < 0 || n >= size w = w
          | otherwise            = case peek w of
                Nothing -> w    -- nothing to do
                Just k  -> insert k n (pop w)

------------------------------------------------------------------------

-- | Insert an element onto the top of stack 'n'.
-- If the index is wrong, the original StackSet is returned unchanged.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
--
insert :: Ord a => a -> Int -> StackSet a -> StackSet a
insert k n old
    | n < 0 || n >= size old = old
    | otherwise              = new { cache  = M.insert k n (cache new)
                                   , stacks = S.adjust (L.nub . (k:)) n (stacks new) }
    where new = delete k old

-- | Delete an element entirely from from the StackSet.
-- This can be used to ensure that a given element is not managed elsewhere.
-- If the element doesn't exist, the original StackSet is returned unmodified.
delete :: Ord a => a -> StackSet a -> StackSet a
delete k w = case M.lookup k (cache w) of
    Nothing -> w -- we don't know about this window
    Just i  -> w { cache  = M.delete k (cache w)
                 , stacks = S.adjust (L.delete k) i (stacks w) }

-- ---------------------------------------------------------------------
-- Internal functions

-- | modify the current stack with a pure function. This function is
-- unsafe: the argument function must only permute the current stack,
-- and must not add or remove elements, or duplicate elements.
--
unsafeModify :: ([a] -> [a]) -> StackSet a -> StackSet a
unsafeModify f w = w { stacks = S.adjust f (cursor w) (stacks w) }

