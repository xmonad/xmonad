-----------------------------------------------------------------------------
-- |
-- Module      :  StackSet
-- Copyright   :  (c) Don Stewart 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  experimental
-- Portability :  portable, Haskell 98
--
-----------------------------------------------------------------------------
--
-- ** Introduction
--
-- The 'StackSet' data type encodes a window manager abstraction. The
-- window manager is a set of virtual workspaces. On each workspace is a
-- stack of windows. A given workspace is always current, and a given
-- window on each workspace has focus. The focused window on the current
-- workspace is the one which will take user input. It can be visualised
-- as follows:
--
--  Workspace  { 0*}   { 1 }   { 2 }   { 3 }   { 4 }
--  
--  Windows    [1      []      [3*     [6*]    []
--             ,2*]            ,4
--                             ,5]
--
-- Note that workspaces are indexed from 0, windows are numbered
-- uniquely. A '*' indicates the window on each workspace that has
-- focus, and which workspace is current.
--
-- ** Zipper 
--
-- We encode all the focus tracking directly in the data structure, with a 'zipper':
--
--    A Zipper is essentially an `updateable' and yet pure functional
--    cursor into a data structure. Zipper is also a delimited
--    continuation reified as a data structure.
--
--    The Zipper lets us replace an item deep in a complex data
--    structure, e.g., a tree or a term, without an  mutation.  The
--    resulting data structure will share as much of its components with
--    the old structure as possible. 
--
--      Oleg Kiselyov, 27 Apr 2005, haskell@, "Zipper as a delimited continuation"
--
-- We use the zipper to keep track of the focused workspace and the
-- focused window on each workspace, allowing us to have correct focus
-- by construction. We closely follow Huet's original implementation:
--
--      G. Huet, /Functional Pearl: The Zipper/,
--      1997, J. Functional Programming 75(5):549-554.
-- and:
--      R. Hinze and J. Jeuring, /Functional Pearl: The Web/.
--
-- and Conor McBride's zipper differentiation paper.
-- Another good reference is:
--
--      The Zipper, Haskell wikibook
-- 
-- ** Xinerama support:
--
-- Xinerama in X11 lets us view multiple virtual workspaces
-- simultaneously. While only one will ever be in focus (i.e. will
-- receive keyboard events), other workspaces may be passively viewable.
-- We thus need to track which virtual workspaces are associated
-- (viewed) on which physical screens. We use a simple Map Workspace
-- Screen for this.
--
-- ** Master and Focus
--
-- Each stack tracks a focused item, and for tiling purposes also tracks
-- a 'master' position. The connection between 'master' and 'focus'
-- needs to be well defined. Particular in relation to 'insert' and
-- 'delete'.
--
module StackSet (
        StackSet(..), Workspace(..), Screen(..), Stack(..),
        new, view, lookupWorkspace, peek, index, focusLeft, focusRight,
        focusWindow, member, findIndex, insertLeft, delete, shift,
        swapMaster, swapLeft, swapRight, modify -- needed by users
    ) where

import Data.Maybe   (listToMaybe)
import qualified Data.List as L (delete,find,genericSplitAt)


-- API changes from xmonad 0.1:
--  StackSet constructor arguments changed. StackSet workspace window screen
--  new,                    -- was: empty
--  view,
--  index,
--  peek,                   -- was: peek/peekStack
--  focusLeft, focusRight,  -- was: rotate
--  swapLeft, swapRight
--  focus                   -- was: raiseFocus
--  insertLeft,             -- was: insert/push
--  delete,
--  swapMaster,             -- was: promote/swap
--  member, 
--  shift,
--  lookupWorkspace,        -- was: workspace
--  visibleWorkspaces       -- gone.
--
------------------------------------------------------------------------

--
-- A cursor into a non-empty list of workspaces. 
-- We puncture the workspace list, producing a hole in the structure
-- used to track the currently focused workspace. The two other lists
-- that are produced are used to track those workspaces visible as
-- Xinerama screens, and those workspaces not visible anywhere.
--
data StackSet i a sid =
    StackSet { size    :: !i                -- number of workspaces
             , current :: !(Screen i a sid) -- currently focused workspace
             , visible :: [Screen i a sid]  -- non-focused workspaces, visible in xinerama
             , hidden  :: [Workspace i a]   -- workspaces not visible anywhere
             } deriving (Show, Read, Eq)

-- Visible workspaces, and their Xinerama screens.
data Screen i a sid = Screen { workspace :: !(Workspace i a), screen :: !sid }
    deriving (Show, Read, Eq)

-- 
-- A workspace is just a tag - its index - and a stack
--
data Workspace i a = Workspace  { tag :: !i, stack :: Stack a }
    deriving (Show, Read, Eq)

-- 
-- A stack is a cursor onto a (possibly empty) window list.
-- The data structure tracks focus by construction, and
-- the master window is by convention the left most item.
-- Focus operations will not reorder the list that results from
-- flattening the cursor.
--
-- A 'Stack' can be viewed as a list with a hole punched in it to make
-- the focused position. Under the zipper/calculus view of such
-- structures, it is the differentiation of a [a], and integrating it
-- back has a natural implementation used in 'index'.
--
data Stack a = Empty
             | Node { focus  :: !a        -- focused thing in this set
                    , left   :: [a]       -- clowns to the left
                    , right  :: [a] }     -- jokers to the right
    deriving (Show, Read, Eq)

-- ---------------------------------------------------------------------
-- Construction

-- | /O(n)/. Create a new stackset, of empty stacks, of size 'n', with
-- 'm' physical screens. 'm' should be less than or equal to 'n'.
-- The workspace with index '0' will be current.
--
-- Xinerama: Virtual workspaces are assigned to physical screens, starting at 0.
--
new :: (Integral i, Integral s) => i -> s -> StackSet i a s
new n m | n > 0 && m > 0 = StackSet n cur visi unseen
        | otherwise      = error "non-positive arguments to StackSet.new"

  where (seen,unseen) = L.genericSplitAt m $ Workspace 0 Empty : [ Workspace i Empty | i <- [1 ..n-1]]
        (cur:visi)    = [ Screen i s |  (i,s) <- zip seen [0..] ]
                -- now zip up visibles with their screen id

-- 
-- /O(w)/. Set focus to the workspace with index 'i'. 
-- If the index is out of range, return the original StackSet.
--
-- Xinerama: If the workspace is not visible on any Xinerama screen, it
-- becomes the current screen. If it is in the visible list, it becomes
-- current.

-- is raised to the current screen. If it is already visible, focus is
-- just moved.
--
view :: (Eq i, Eq a, Eq s, Integral i) => i -> StackSet i a s -> StackSet i a s
view i s
    | i < 0 && i >= size s || i == tag (workspace (current s)) = s  -- out of bounds or current

    | Just x <- L.find ((i==).tag.workspace) (visible s)
    -- if it is visible, it is just raised
    = s { current = x, visible = current s : L.delete x (visible s) }

    | Just x <- L.find ((i==).tag)     (hidden  s)
    -- if it was hidden, it is raised on the xine screen currently used
    = s { current = Screen x (screen (current s))
        , hidden = workspace (current s) : L.delete x (hidden s) }

    | otherwise = error "Inconsistent StackSet: workspace not found"

    -- 'Catch'ing this might be hard. Relies on monotonically increasing
    -- workspace tags defined in 'new'

-- ---------------------------------------------------------------------
-- Xinerama operations

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- Nothing if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i a s -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s <- current w : visible w, s == sc ]

-- ---------------------------------------------------------------------
-- Operations on the current stack

--
-- The 'with' function takes a default value, a function, and a
-- StackSet. If the current stack is Empty, 'with' returns the
-- default value. Otherwise, it applies the function to the stack,
-- returning the result. It is like 'maybe' for the focused workspace.
--
with :: b -> (Stack a -> b) -> StackSet i a s -> b
with dflt f s = case stack (workspace (current s)) of Empty -> dflt; v -> f v
    -- TODO: ndm: a 'catch' proof here that 'f' only gets Node
    --            constructors, hence all 'f's are safe below?

--
-- Apply a function, and a default value for Empty, to modify the current stack.
--
modify :: Stack a -> (Stack a -> Stack a) -> StackSet i a s -> StackSet i a s
modify d f s = s { current = (current s)
                        { workspace = (workspace (current s)) { stack = with d f s }}}

--
-- /O(1)/. Extract the focused element of the current stack. 
-- Return Just that element, or Nothing for an empty stack.
--
peek :: StackSet i a s -> Maybe a
peek = with Nothing (return . focus)

--
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
--
index :: Eq a => StackSet i a s -> [a]
index = with [] $ \(Node t l r) -> reverse l ++ t : r

--  let is = t : r ++ reverse l in take (length is) (dropWhile (/= m) (cycle is))

--
-- /O(1), O(w) on the wrapping case/. 
--
-- focusLeft, focusRight. Move the window focus left or
-- right, wrapping if we reach the end. The wrapping should model a
-- 'cycle' on the current stack. The 'master' window, and window order,
-- are unaffected by movement of focus.
--
-- swapLeft, swapRight. Swap the focused window with its left or right
-- neighbour in the stack ordering, wrapping if we reach the end. Again 
-- the wrapping model should 'cycle' on the current stack.
-- 
focusLeft, focusRight, swapLeft, swapRight :: StackSet i a s -> StackSet i a s
focusLeft = modify Empty $ \c -> case c of
    Node _ []     [] -> c
    Node t (l:ls) rs -> Node l ls (t:rs)
    Node t []     rs -> Node x (xs ++ [t]) [] where (x:xs) = reverse rs

focusRight = modify Empty $ \c -> case c of
    Node _ []     [] -> c
    Node t ls (r:rs) -> Node r (t:ls) rs
    Node t ls     [] -> Node x [] (xs ++ [t]) where (x:xs) = reverse ls

swapLeft = modify Empty $ \c -> case c of
    Node _ []     [] -> c
    Node t (l:ls) rs -> Node t ls (l:rs)
    Node t []     rs -> Node t (reverse rs) []

swapRight = modify Empty $ \c -> case c of
    Node _ []     [] -> c
    Node t ls (r:rs) -> Node t (r:ls) rs
    Node t ls     [] -> Node t [] (reverse ls)

--
-- | /O(1) on current window, O(n) in general/. Focus the window 'w', 
-- and set its workspace as current.
--
focusWindow :: (Integral i, Eq s, Eq a) => a -> StackSet i a s -> StackSet i a s
focusWindow w s | Just w == peek s = s
                | otherwise        = maybe s id $ do
                    n <- findIndex w s
                    return $ until ((Just w ==) . peek) focusLeft (view n s)

--
-- Finding if a window is in the stackset is a little tedious. We could
-- keep a cache :: Map a i, but with more bookkeeping.
--

-- | /O(n)/. Is a window in the StackSet.
member :: Eq a => a -> StackSet i a s -> Bool
member a s = maybe False (const True) (findIndex a s)

-- | /O(1) on current window, O(n) in general/.
-- Return Just the workspace index of the given window, or Nothing
-- if the window is not in the StackSet.
findIndex :: Eq a => a -> StackSet i a s -> Maybe i
findIndex a s = listToMaybe
    [ tag w | w <- workspace (current s) : map workspace (visible s) ++ hidden s, has a (stack w) ]
    where has _ Empty         = False
          has x (Node t l r) = x `elem` (t : l ++ r)

-- ---------------------------------------------------------------------
-- Modifying the stackset

--
-- /O(n)/. (Complexity due to duplicate check). Insert a new element into
-- the stack, to the left of the currently focused element.
--
-- The new element is given focus, and is set as the master window.
-- The previously focused element is moved to the right.  The previously
-- 'master' element is forgotten. (Thus, 'insert' will cause a retiling).
--
-- If the element is already in the stackset, the original stackset is
-- returned unmodified.
--
-- Semantics in Huet's paper is that insert doesn't move the cursor.
-- However, we choose to insert to the left, and move the focus.
--
insertLeft :: Eq a => a -> StackSet i a s -> StackSet i a s
insertLeft a s = if member a s then s else insert
  where insert = modify (Node a [] []) (\(Node t l r) -> Node a l (t:r)) s

-- insertRight :: a -> StackSet i a s -> StackSet i a s
-- insertRight a = modify (Node a [] []) $ \(Node t l r) -> Node a (t:l) r
-- Old semantics, from Huet.
-- >    w { right = a : right w }

--
-- /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
-- There are 4 cases to consider:
--
--   * delete on an Empty workspace leaves it Empty
--   * otherwise, try to move focus to the right
--   * otherwise, try to move focus to the left
--   * otherwise, you've got an empty workspace, becomes Empty
--
-- Behaviour with respect to the master:
--
--   * deleting the master window resets it to the newly focused window
--   * otherwise, delete doesn't affect the master.
--
delete :: (Integral i, Eq a, Eq s) => a -> StackSet i a s -> StackSet i a s
delete w s | Just w == peek s = remove s -- common case. 
           | otherwise = maybe s (removeWindow.tag.workspace.current $ s) (findIndex w s)
  where
    -- find and remove window script
    removeWindow o n = foldr ($) s [view o,remove,view n]

    -- actual removal logic, and focus/master logic:
    remove = modify Empty $ \c -> 
      if focus c == w 
      then case c of
        Node _ ls     (r:rs) -> Node r ls rs    -- try right first
        Node _ (l:ls) []     -> Node l ls []    -- else left.
        Node _ []     []     -> Empty
      else c { left  = w `L.delete` left c, right = w `L.delete` right c }

------------------------------------------------------------------------
-- Setting the master window

-- /O(s)/. Set the master window to the focused window.
-- The old master window is swapped in the tiling order with the focused window.
-- Focus stays with the item moved.
swapMaster :: StackSet i a s -> StackSet i a s
swapMaster = modify Empty $ \c -> case c of
    Node _ [] _  -> c    -- already master.
    Node t ls rs -> Node t [] (ys ++ x : rs) where (x:ys) = reverse ls

 -- natural! keep focus, move current to furthest left, move furthest
-- left to current position.

-- ---------------------------------------------------------------------
-- Composite operations
--

-- /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted to the left of the currently focused element on that
-- workspace.  The actual focused workspace doesn't change. If there is
-- no element on the current stack, the original stackSet is returned.
--
shift :: (Eq a, Eq s, Integral i) => i -> StackSet i a s -> StackSet i a s
shift n s = if and [n >= 0,n < size s,n /= tag (workspace (current s))]
            then maybe s go (peek s) else s
    where go w = foldr ($) s [view (tag (workspace (current s))),insertLeft w,view n,delete w]
                           -- ^^ poor man's state monad :-)

