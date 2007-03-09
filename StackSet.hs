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
-- A StackSet provides a nice data structure for multiscreen
-- window managers, where each screen has a stack of windows, and a window
-- may be on only 1 screen at any given time.
--

module StackSet (

    StackSet,           -- abstract, deriving Show,Eq

    -- * Introduction
    empty,              -- :: Int -> StackSet a
    fromList,           -- :: Ord a => (Int,[[a]]) -> StackSet a
    toList,             -- :: StackSet a -> (Int,[[a]])

    -- * Inspection
    size,               -- :: StackSet a -> Int
    peek,               -- :: StackSet a -> Maybe a
    index,              -- :: Int -> StackSet a -> [a]
    member,             -- :: Ord a => a -> StackSet a -> Bool
    current,            -- :: StackSet a -> Int

    -- * Modification
    push,               -- :: Ord a => a -> StackSet a -> StackSet a
    rotate,             -- :: Ordering -> StackSet a -> StackSet a
    shift,              -- :: Ord a => Int -> StackSet a -> StackSet a
    delete,             -- :: Ord a => a -> StackSet a -> StackSet a
    view,               -- :: Int -> StackSet a -> StackSet a

  ) where

import Data.Maybe
import qualified Data.List     as L
import qualified Data.Map      as M
import qualified Data.IntMap   as I

------------------------------------------------------------------------

-- | The StackSet data structure. A table of stacks, with a current pointer
data StackSet a =
    StackSet
        { current:: {-# UNPACK #-} !Int             -- ^ the currently visible stack
        , size   :: {-# UNPACK #-} !Int             -- ^ size of the stack list
        , stacks :: {-# UNPACK #-} !(I.IntMap [a])  -- ^ the separate stacks
        , cache  :: {-# UNPACK #-} !(M.Map a Int)   -- ^ a cache of windows back to their stacks
        } deriving Eq

instance Show a => Show (StackSet a) where
    showsPrec p s r = showsPrec p (show . toList $ s) r

-- Ord a constraint on 'a' as we use it as a key.
--
-- The cache is used to check on insertion that we don't already have
-- this window managed on another stack
--
-- Currently stacks are of a fixed size. There's no firm reason to
-- do this (new empty stacks could be created on the fly).

------------------------------------------------------------------------

-- | /O(n)/. Create a new empty stacks of size 'n', indexed from 0. The
-- 0-indexed stack will be current.
empty :: Int -> StackSet a
empty n = StackSet { current= 0
                   , size   = n -- constant
                   , stacks = I.fromList (zip [0..n-1] (repeat []))
                   , cache  = M.empty
                   }

-- | /O(log w)/. True if x is somewhere in the StackSet
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
toList x = (current x, map snd $ I.toList (stacks x))

------------------------------------------------------------------------

-- | Push. Insert an element onto the top of the current stack. 
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that
-- stack first.
push :: Ord a => a -> StackSet a -> StackSet a
push k w = insert k (current w) w

-- | /O(log s)/. Extract the element on the top of the current stack. If no such
-- element exists, Nothing is returned.
peek :: StackSet a -> Maybe a
peek w = listToMaybe $ index (current w) w

-- | /O(log s)/. Index. Extract the stack at index 'n'.
-- If the index is invalid, an exception is thrown.
index :: Int -> StackSet a -> [a]
index k w = fromJust (I.lookup k (stacks w))

-- | /O(1)/. view. Set the stack specified by the Int argument as being the
-- current StackSet. If the index is out of range an exception is thrown.
view :: Int -> StackSet a -> StackSet a
view n w | n >= 0 && n < size w = w { current = n }
         | otherwise            = error $ "view: index out of bounds: " ++ show n

-- | /O(log n)/. rotate. cycle the current window list up or down.
--
--  rotate EQ   -->  [5,6,7,8,1,2,3,4]
--  rotate GT   -->  [6,7,8,1,2,3,4,5]
--  rotate LT   -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: Ordering -> StackSet a -> StackSet a
rotate o w = w { stacks = I.adjust rot (current w) (stacks w) }
    where
        rot s = take l . drop offset . cycle $ s
           where
              n      = fromEnum o - 1
              l      = length s
              offset = if n < 0 then l + n else n

-- | /O(log n)/. shift. move the client on top of the current stack to
-- the top of stack 'n'. If the stack to move to is not valid, and
-- exception is thrown.
--
shift :: Ord a => Int -> StackSet a -> StackSet a
shift n w = maybe w (\k -> insert k n (delete k w)) (peek w)

-- | /O(log n)/. Insert an element onto the top of stack 'n'.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
-- If the index is wrong an exception is thrown.
--
insert :: Ord a => a -> Int -> StackSet a -> StackSet a
insert k n old = new { cache  = M.insert k n (cache new)
                     , stacks = I.adjust (L.nub . (k:)) n (stacks new) }
    where new = delete k old

-- | /O(log n)/. Delete an element entirely from from the StackSet.
-- This can be used to ensure that a given element is not managed elsewhere.
-- If the element doesn't exist, the original StackSet is returned unmodified.
delete :: Ord a => a -> StackSet a -> StackSet a
delete k w = maybe w tweak (M.lookup k (cache w))
  where tweak i = w { cache  = M.delete k (cache w)
                    , stacks = I.adjust (L.delete k) i (stacks w) }
