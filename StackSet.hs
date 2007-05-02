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
-- A StackSet provides a nice data structure for window managers with
-- multiple physical screens, and multiple workspaces, where each screen
-- has a stack of windows, and a window may be on only 1 screen at any
-- given time.
--

module StackSet (
    StackSet(..),           -- abstract

    screen, peekStack, index, empty, peek, push, delete, member,
    raiseFocus, rotate, promote, shift, view, workspace, insert,
    visibleWorkspaces, swap {- helper -}
  ) where

import Data.Maybe
import qualified Data.List as L (delete,elemIndex)
import qualified Data.Map  as M

------------------------------------------------------------------------

-- | The StackSet data structure. Multiple screens containing tables of
-- stacks, with a current pointer
data StackSet i j a =
    StackSet
        { current  :: !i                    -- ^ the currently visible stack
        , screen2ws:: !(M.Map j i)          -- ^ screen -> workspace
        , ws2screen:: !(M.Map i j)          -- ^ workspace -> screen map
        , stacks   :: !(M.Map i ([a], [a])) -- ^ screen -> (floating, normal)
        , focus    :: !(M.Map i a)          -- ^ the window focused in each stack
        , cache    :: !(M.Map a i)          -- ^ a cache of windows back to their stacks
        } deriving (Eq, Show)

-- The cache is used to check on insertion that we don't already have
-- this window managed on another stack

------------------------------------------------------------------------

-- | /O(n)/. Create a new stackset, of empty stacks, of size 'n',
-- indexed from 0, with 'm' screens. (also indexed from 0) The 0-indexed
-- stack will be current.
empty :: (Integral i, Integral j) => Int -> Int -> StackSet i j a
empty n m = StackSet { current   = 0
                     , screen2ws = wsScrs2Works
                     , ws2screen = wsWorks2Scrs
                     , stacks    = M.fromList (zip [0..fromIntegral n-1] (repeat ([], [])))
                     , focus     = M.empty
                     , cache     = M.empty }

    where (scrs,wrks)  = unzip $ map (\x -> (fromIntegral x, fromIntegral x)) [0..m-1]
          wsScrs2Works = M.fromList (zip scrs wrks)
          wsWorks2Scrs = M.fromList (zip wrks scrs)

-- | /O(log w)/. True if x is somewhere in the StackSet
member :: Ord a => a -> StackSet i j a -> Bool
member a w = M.member a (cache w)

-- | /O(log n)/. Looks up the workspace that x is in, if it is in the StackSet
-- lookup :: (Monad m, Ord a) => a -> StackSet i j a -> m i
-- lookup x w = M.lookup x (cache w)

-- | /O(n)/. Number of stacks
-- size :: StackSet i j a -> Int
-- size = M.size . stacks

------------------------------------------------------------------------

-- | Push. Insert an element onto the top of the current stack.
-- If the element is already in the current stack, it is moved to the top.
-- If the element is managed on another stack, it is removed from that
-- stack first.
push :: (Integral i, Ord a) => a -> StackSet i j a -> StackSet i j a
push k w = insert k (current w) w

-- | /O(log s)/. Extract the element on the top of the current stack. If no such
-- element exists, Nothing is returned.
peek :: Integral i => StackSet i j a -> Maybe a
peek w = peekStack (current w) w

-- | /O(log s)/. Extract the element on the top of the given stack. If no such
-- element exists, Nothing is returned.
peekStack :: Integral i => i -> StackSet i j a -> Maybe a
peekStack i w = M.lookup i (focus w)

-- | /O(log s)/. Index. Extract the stack at workspace 'n'.
-- If the index is invalid, an exception is thrown.
index :: Integral i => i -> StackSet i j a -> [a]
index k w = uncurry (++) $ fromJust $ M.lookup k (stacks w)

-- | view. Set the stack specified by the argument as being visible and the
-- current StackSet. If the stack wasn't previously visible, it will become
-- visible on the current screen. If the index is out of range an exception is
-- thrown.
view :: (Integral i, Integral j) => i -> StackSet i j a -> StackSet i j a
view n w | M.member n (stacks w)
         = if M.member n (ws2screen w) then w { current = n }
                                       else tweak (fromJust $ screen (current w) w)
         | otherwise = error $ "view: index out of bounds: " ++ show n
  where
    tweak sc = w { screen2ws = M.insert sc n (screen2ws w)
                 , ws2screen = M.insert n sc (M.filter (/=sc) (ws2screen w))
                 , current = n }

-- | That screen that workspace 'n' is visible on, if any.
screen :: Integral i => i -> StackSet i j a -> Maybe j
screen n w = M.lookup n (ws2screen w)

-- | The workspace visible on screen 'sc'. Nothing if screen is out of bounds.
workspace :: Integral j => j -> StackSet i j a -> Maybe i
workspace sc w = M.lookup sc (screen2ws w)

-- | A list of the currently visible workspaces.
visibleWorkspaces :: StackSet i j a -> [i]
visibleWorkspaces = M.keys . ws2screen

--
-- | /O(log n)/. rotate. cycle the current window list up or down.
-- Has the effect of rotating focus. In fullscreen mode this will cause
-- a new window to be visible.
--
--  rotate EQ   -->  [5,6,7,8,1,2,3,4]
--  rotate GT   -->  [6,7,8,1,2,3,4,5]
--  rotate LT   -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: (Integral i, Eq a) => Ordering -> StackSet i j a -> StackSet i j a
rotate o w = maybe w id $ do
    f <- M.lookup (current w) (focus w)
    s <- fmap (uncurry (++)) $ M.lookup (current w) (stacks w)
    ea <- case o of EQ -> Nothing
                    _  -> elemAfter f (if o == GT then s else reverse s)
    return $ w { focus = M.insert (current w) ea (focus w) }

-- | /O(log n)/. shift. move the client on top of the current stack to
-- the top of stack 'n'. If the stack to move to is not valid, and
-- exception is thrown.
--
shift :: (Integral i, Ord a) => i -> StackSet i j a -> StackSet i j a
shift n w = maybe w (\k -> insert k n w) (peek w)

-- | /O(log n)/. Insert an element onto the top of stack 'n'.
-- If the element is already in the stack 'n', it is moved to the top.
-- If the element exists on another stack, it is removed from that stack.
-- If the index is wrong an exception is thrown.
--
insert :: (Integral i, Ord a) => a -> i -> StackSet i j a -> StackSet i j a
insert k n old = new { cache  = M.insert k n (cache new)
                     , stacks = M.adjust (\(f, ks) -> (f, k:ks)) n (stacks new)
                     , focus  = M.insert n k (focus new) }
    where new = delete k old

-- | /O(log n)/. Delete an element entirely from from the StackSet.
-- This can be used to ensure that a given element is not managed elsewhere.
-- If the element doesn't exist, the original StackSet is returned unmodified.
delete :: (Integral i, Ord a) => a -> StackSet i j a -> StackSet i j a
delete k w = maybe w del (M.lookup k (cache w))
  where
    del i = w { cache  = M.delete k (cache w)
              , stacks = M.adjust (\(xs, ys) -> (L.delete k xs, L.delete k ys)) i (stacks w)
              , focus  = M.update (\k' -> if k == k' then elemAfter k (index i w)
                                                     else Just k') i (focus w) }

-- | /O(log n)/. If the given window is contained in a workspace, make it the
-- focused window of that workspace, and make that workspace the current one.
raiseFocus :: (Integral i, Integral j, Ord a) => a -> StackSet i j a -> StackSet i j a
raiseFocus k w = case M.lookup k (cache w) of
    Nothing -> w
    Just i  -> (view i w) { focus = M.insert i k (focus w) }

-- | Swap the currently focused window with the master window (the
-- window on top of the stack). Focus moves to the master.
promote :: (Integral i, Ord a) => StackSet i j a -> StackSet i j a
promote w = maybe w id $ do
    a <- peek w -- fail if null
    let w' = w { stacks = M.adjust (\(f, s) -> (f, swap a (head s) s)) (current w) (stacks w) }
    return $ insert a (current w) w' -- and maintain focus (?)

-- | Swap first occurences of 'a' and 'b' in list.
-- If both elements are not in the list, the list is unchanged.
--
-- Given a set as a list (no duplicates)
--
-- > swap a b . swap a b == id
--
swap :: Eq a => a -> a -> [a] -> [a]
swap a b xs | a == b  = xs    -- do nothing
            | Just ai <- L.elemIndex a xs
            , Just bi <- L.elemIndex b xs = insertAt bi a (insertAt ai b xs)
  where insertAt n x ys = as ++ x : tail bs
            where (as,bs) = splitAt n ys
swap _ _ xs = xs -- do nothing

--
-- cycling:
-- promote w = w { stacks = M.adjust next (current w) (stacks w) }
--    where next [] = []
--          next xs = last xs : init xs
--

-- | Returns true if the window is in the floating layer
isFloat :: (Ord a, Ord i) => a -> StackSet i j a -> Bool
isFloat k w = maybe False (elem k . fst . (stacks w M.!)) (M.lookup k (cache w))

-- | Find the element in the (circular) list after given element.
elemAfter :: Eq a => a -> [a] -> Maybe a
elemAfter w ws = listToMaybe . filter (/= w) . dropWhile (/= w) $ ws ++ ws
