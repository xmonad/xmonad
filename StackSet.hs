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

module StackSet (
        -- * Introduction
        -- $intro
        StackSet(..), Workspace(..), Screen(..), StackOrNot, Stack(..), RationalRect(..),
        -- *  Construction
        -- $construction
        new, view,
        -- * Xinerama operations
        -- $xinerama
        lookupWorkspace,
        -- *  Operations on the current stack
        -- $stackOperations
        peek, index, integrate, integrate', differentiate,
        focusUp, focusDown,
        focusWindow, tagMember, member, findIndex,
        -- * Modifying the stackset
        -- $modifyStackset
        insertUp, delete, filter,
        -- * Setting the master window
        -- $settingMW
        swapMaster, swapUp, swapDown, modify, modify', float, sink, -- needed by users
        -- * Composite operations
        -- $composite
        shift
    ) where

import Prelude hiding (filter)
import Data.Maybe   (listToMaybe)
import qualified Data.List as L (delete,deleteBy,find,splitAt,filter)
import qualified Data.Map  as M (Map,insert,delete,empty)

-- $intro
--
-- The 'StackSet' data type encodes a window manager abstraction. The
-- window manager is a set of virtual workspaces. On each workspace is a
-- stack of windows. A given workspace is always current, and a given
-- window on each workspace has focus. The focused window on the current
-- workspace is the one which will take user input. It can be visualised
-- as follows:
-- 
-- > Workspace  { 0*}   { 1 }   { 2 }   { 3 }   { 4 }
-- > 
-- > Windows    [1      []      [3*     [6*]    []
-- >            ,2*]            ,4
-- >                            ,5]
-- 
-- Note that workspaces are indexed from 0, windows are numbered
-- uniquely. A '*' indicates the window on each workspace that has
-- focus, and which workspace is current.
--
-- Zipper 
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
--      Oleg Kiselyov, 27 Apr 2005, haskell\@, "Zipper as a delimited continuation"
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
-- Xinerama support:
--
-- Xinerama in X11 lets us view multiple virtual workspaces
-- simultaneously. While only one will ever be in focus (i.e. will
-- receive keyboard events), other workspaces may be passively viewable.
-- We thus need to track which virtual workspaces are associated
-- (viewed) on which physical screens. We use a simple Map Workspace
-- Screen for this.
--
-- Master and Focus
--
-- Each stack tracks a focused item, and for tiling purposes also tracks
-- a 'master' position. The connection between 'master' and 'focus'
-- needs to be well defined. Particular in relation to 'insert' and
-- 'delete'.
--

-- | 
-- API changes from xmonad 0.1:
-- StackSet constructor arguments changed. StackSet workspace window screen
--
-- * new,                    -- was: empty
--
-- * view,
--
-- * index,
--
-- * peek,                   -- was: peek\/peekStack
--
-- * focusUp, focusDown,  -- was: rotate
--
-- * swapUp, swapDown
--
-- * focus                   -- was: raiseFocus
--
-- * insertUp,             -- was: insert\/push
--
-- * delete,
--
-- * swapMaster,             -- was: promote\/swap
--
-- * member, 
--
-- * shift,
--
-- * lookupWorkspace,        -- was: workspace
--
-- * visibleWorkspaces       -- gone.
--
------------------------------------------------------------------------
-- |
-- A cursor into a non-empty list of workspaces. 
-- 
-- We puncture the workspace list, producing a hole in the structure
-- used to track the currently focused workspace. The two other lists
-- that are produced are used to track those workspaces visible as
-- Xinerama screens, and those workspaces not visible anywhere.

data StackSet i a sid sd =
    StackSet { current  :: !(Screen i a sid sd)    -- ^ currently focused workspace
             , visible  :: [Screen i a sid sd]     -- ^ non-focused workspaces, visible in xinerama
             , hidden   :: [Workspace i a]      -- ^ workspaces not visible anywhere
             , floating :: M.Map a RationalRect -- ^ floating windows
             } deriving (Show, Read, Eq)

-- | Visible workspaces, and their Xinerama screens.
data Screen i a sid sd = Screen { workspace :: !(Workspace i a)
                                , screen :: !sid
                                , screenDetail :: !sd }
    deriving (Show, Read, Eq)

-- |
-- A workspace is just a tag - its index - and a stack
--
data Workspace i a = Workspace  { tag :: !i, stack :: StackOrNot a }
    deriving (Show, Read, Eq)

data RationalRect = RationalRect Rational Rational Rational Rational
    deriving (Show, Read, Eq)

-- |
-- A stack is a cursor onto a (possibly empty) window list.
-- The data structure tracks focus by construction, and
-- the master window is by convention the top-most item.
-- Focus operations will not reorder the list that results from
-- flattening the cursor. The structure can be envisaged as:
--
-- >    +-- master:  < '7' >
-- > up |            [ '2' ]
-- >    +---------   [ '3' ]
-- > focus:          < '4' >
-- > dn +----------- [ '8' ]
--
-- A 'Stack' can be viewed as a list with a hole punched in it to make
-- the focused position. Under the zipper\/calculus view of such
-- structures, it is the differentiation of a [a], and integrating it
-- back has a natural implementation used in 'index'.
--
type StackOrNot a = Maybe (Stack a)

data Stack a = Stack { focus  :: !a        -- focused thing in this set
                     , up     :: [a]       -- clowns to the left
                     , down   :: [a] }     -- jokers to the right
    deriving (Show, Read, Eq)


-- | this function indicates to catch that an error is expected
abort :: String -> a
abort x = error $ "xmonad: StackSet: " ++ x

-- ---------------------------------------------------------------------
-- $construction

-- | /O(n)/. Create a new stackset, of empty stacks, with given tags, with
-- 'm' physical screens. 'm' should be less than or equal to the number of
-- workspace tags.  The first workspace in the list will be current.
--
-- Xinerama: Virtual workspaces are assigned to physical screens, starting at 0.
--
new :: (Integral s) => [i] -> [sd] -> StackSet i a s sd
new wids m | not (null wids) && length m <= length wids = StackSet cur visi unseen M.empty
  where (seen,unseen) = L.splitAt (length m) $ map (flip Workspace Nothing) wids
        (cur:visi)    = [ Screen i s sd |  (i, s, sd) <- zip3 seen [0..] m ]
                -- now zip up visibles with their screen id
new _ _ = abort "non-positive argument to StackSet.new"



-- |
-- /O(w)/. Set focus to the workspace with index \'i\'. 
-- If the index is out of range, return the original StackSet.
--
-- Xinerama: If the workspace is not visible on any Xinerama screen, it
-- becomes the current screen. If it is in the visible list, it becomes
-- current.

view :: (Eq a, Eq s, Eq i) => i -> StackSet i a s sd -> StackSet i a s sd
view i s
    | not (elem i $ map tag $ workspaces s)
      || i == tag (workspace (current s)) = s  -- out of bounds or current

    | Just x <- L.find ((i==).tag.workspace) (visible s)
    -- if it is visible, it is just raised
    = s { current = x, visible = current s : L.deleteBy screenEq x (visible s) }

    | Just x <- L.find ((i==).tag)           (hidden  s)
    -- if it was hidden, it is raised on the xine screen currently used
    = s { current = (current s) { workspace = x }
        , hidden = workspace (current s) : L.delete x (hidden s) }

    | otherwise = s
 where screenEq x y = screen x == screen y

    -- 'Catch'ing this might be hard. Relies on monotonically increasing
    -- workspace tags defined in 'new'

-- ---------------------------------------------------------------------
-- $xinerama

-- | Find the tag of the workspace visible on Xinerama screen 'sc'.
-- Nothing if screen is out of bounds.
lookupWorkspace :: Eq s => s -> StackSet i a s sd -> Maybe i
lookupWorkspace sc w = listToMaybe [ tag i | Screen i s _ <- current w : visible w, s == sc ]

-- ---------------------------------------------------------------------
-- $stackOperations

-- |
-- The 'with' function takes a default value, a function, and a
-- StackSet. If the current stack is Nothing, 'with' returns the
-- default value. Otherwise, it applies the function to the stack,
-- returning the result. It is like 'maybe' for the focused workspace.
--
with :: b -> (Stack a -> b) -> StackSet i a s sd -> b
with dflt f = maybe dflt f . stack . workspace . current

-- |
-- Apply a function, and a default value for Nothing, to modify the current stack.
--
modify :: StackOrNot a -> (Stack a -> StackOrNot a) -> StackSet i a s sd -> StackSet i a s sd
modify d f s = s { current = (current s)
                        { workspace = (workspace (current s)) { stack = with d f s }}}

-- |
-- Apply a function to modify the current stack if it isn't empty, and we don't
--  want to empty it.
--
modify' :: (Stack a -> Stack a) -> StackSet i a s sd -> StackSet i a s sd
modify' f = modify Nothing (Just . f)

-- |
-- /O(1)/. Extract the focused element of the current stack. 
-- Return Just that element, or Nothing for an empty stack.
--
peek :: StackSet i a s sd -> Maybe a
peek = with Nothing (return . focus)

-- |
-- /O(n)/. Flatten a Stack into a list.
--
integrate :: Stack a -> [a]
integrate (Stack x l r) = reverse l ++ x : r

-- |
-- /O(n)/ Flatten a possibly empty stack into a list.
integrate' :: StackOrNot a -> [a]
integrate' = maybe [] integrate

-- |
-- /O(n)/. Texture a list.
--
differentiate :: [a] -> StackOrNot a
differentiate [] = Nothing
differentiate (x:xs) = Just $ Stack x [] xs

-- |
-- /O(n)/. 'filter p s' returns the elements of 's' such that 'p' evaluates to
-- True.  Order is preserved, and focus moves to the next node to the right (if
-- necessary).
--
-- Note, this isn't the same as the 'remove' semantics, as focus
-- won't move 'left' on the end of list.
--
filter :: (a -> Bool) -> Stack a -> StackOrNot a
filter p (Stack f ls rs) = case L.filter p (f:rs) of
    f':rs' -> Just $ Stack f' (L.filter p ls) rs'    -- maybe move focus down
    []     -> case L.filter p (reverse ls) of        -- filter back up
                    f':rs' -> Just $ Stack f' [] rs' -- else up
                    []     -> Nothing

-- |
-- /O(s)/. Extract the stack on the current workspace, as a list.
-- The order of the stack is determined by the master window -- it will be
-- the head of the list. The implementation is given by the natural
-- integration of a one-hole list cursor, back to a list.
--
index :: Eq a => StackSet i a s sd -> [a]
index = with [] integrate

--  let is = t : r ++ reverse l in take (length is) (dropWhile (/= m) (cycle is))

-- |
-- /O(1), O(w) on the wrapping case/. 
--
-- focusUp, focusDown. Move the window focus up or down the stack,
-- wrapping if we reach the end. The wrapping should model a -- 'cycle'
-- on the current stack. The 'master' window, and window order,
-- are unaffected by movement of focus.
--
-- swapUp, swapDown, swap the neighbour in the stack ordering, wrapping
-- if we reach the end. Again the wrapping model should 'cycle' on
-- the current stack.
-- 
focusUp, focusDown, swapUp, swapDown :: StackSet i a s sd -> StackSet i a s sd
focusUp   = modify' focusUp'
focusDown = modify' (reverseStack . focusUp' . reverseStack)

swapUp    = modify' swapUp'
swapDown  = modify' (reverseStack . swapUp' . reverseStack)

focusUp', swapUp' :: Stack a -> Stack a
focusUp' (Stack t (l:ls) rs) = Stack l ls (t:rs)
focusUp' (Stack t []     rs) = Stack x xs [] where (x:xs) = reverse (t:rs)

swapUp'  (Stack t (l:ls) rs) = Stack t ls (l:rs)
swapUp'  (Stack t []     rs) = Stack t (reverse rs) []

-- | reverse a stack: up becomes down and down becomes up.
reverseStack :: Stack a -> Stack a
reverseStack (Stack t ls rs) = Stack t rs ls

--
-- | /O(1) on current window, O(n) in general/. Focus the window 'w', 
-- and set its workspace as current.
--
focusWindow :: (Integral i, Eq s, Eq a) => a -> StackSet i a s sd -> StackSet i a s sd
focusWindow w s | Just w == peek s = s
                | otherwise        = maybe s id $ do
                    n <- findIndex w s
                    return $ until ((Just w ==) . peek) focusUp (view n s)



-- | Get a list of all workspaces in the StackSet.
workspaces :: StackSet i a s sd -> [Workspace i a]
workspaces s = workspace (current s) : map workspace (visible s) ++ hidden s

-- | Is the given tag present in the StackSet?
tagMember :: Eq i => i -> StackSet i a s sd -> Bool
tagMember t = elem t . map tag . workspaces

-- |
-- Finding if a window is in the stackset is a little tedious. We could
-- keep a cache :: Map a i, but with more bookkeeping.
--

-- | /O(n)/. Is a window in the StackSet.
member :: Eq a => a -> StackSet i a s sd -> Bool
member a s = maybe False (const True) (findIndex a s)

-- | /O(1) on current window, O(n) in general/.
-- Return Just the workspace index of the given window, or Nothing
-- if the window is not in the StackSet.
findIndex :: Eq a => a -> StackSet i a s sd -> Maybe i
findIndex a s = listToMaybe
    [ tag w | w <- workspaces s, has a (stack w) ]
    where has _ Nothing         = False
          has x (Just (Stack t l r)) = x `elem` (t : l ++ r)

-- ---------------------------------------------------------------------
-- $modifyStackset
 
-- |
-- /O(n)/. (Complexity due to duplicate check). Insert a new element into
-- the stack, above the currently focused element.
--
-- The new element is given focus, and is set as the master window.
-- The previously focused element is moved down.  The previously
-- 'master' element is forgotten. (Thus, 'insert' will cause a retiling).
--
-- If the element is already in the stackset, the original stackset is
-- returned unmodified.
--
-- Semantics in Huet's paper is that insert doesn't move the cursor.
-- However, we choose to insert above, and move the focus.
--
insertUp :: Eq a => a -> StackSet i a s sd -> StackSet i a s sd
insertUp a s = if member a s then s else insert
  where insert = modify (Just $ Stack a [] []) (\(Stack t l r) -> Just $ Stack a l (t:r)) s

-- insertDown :: a -> StackSet i a s sd -> StackSet i a s sd
-- insertDown a = modify (Stack a [] []) $ \(Stack t l r) -> Stack a (t:l) r
-- Old semantics, from Huet.
-- >    w { down = a : down w }

-- |
-- /O(1) on current window, O(n) in general/. Delete window 'w' if it exists.
-- There are 4 cases to consider:
--
--   * delete on an Nothing workspace leaves it Nothing
--   * otherwise, try to move focus to the down
--   * otherwise, try to move focus to the up
--   * otherwise, you've got an empty workspace, becomes Nothing
--
-- Behaviour with respect to the master:
--
--   * deleting the master window resets it to the newly focused window
--   * otherwise, delete doesn't affect the master.
--
delete :: (Integral i, Ord a, Eq s) => a -> StackSet i a s sd -> StackSet i a s sd
delete w s | Just w == peek s = remove s -- common case. 
           | otherwise = maybe s (removeWindow.tag.workspace.current $ s) (findIndex w s)
  where
    -- find and remove window script
    removeWindow o n = foldr ($) s [view o,remove,view n]

    -- actual removal logic, and focus/master logic:
    remove = modify Nothing $ \c ->
      if focus c == w
      then case c of
        Stack _ ls     (r:rs) -> Just $ Stack r ls rs    -- try down first
        Stack _ (l:ls) []     -> Just $ Stack l ls []    -- else up
        Stack _ []     []     -> Nothing
      else Just $ c { up = w `L.delete` up c, down = w `L.delete` down c }

------------------------------------------------------------------------

-- | Given a window, and its preferred rectangle, set it as floating
-- A floating window should already be managed by the StackSet.
float :: Ord a => a -> RationalRect -> StackSet i a s sd -> StackSet i a s sd
float w r s = s { floating = M.insert w r (floating s) }

-- | Clear the floating status of a window
sink :: Ord a => a -> StackSet i a s sd -> StackSet i a s sd
sink w s = s { floating = M.delete w (floating s) }

------------------------------------------------------------------------
-- $settingMW
 
-- | /O(s)/. Set the master window to the focused window.
-- The old master window is swapped in the tiling order with the focused window.
-- Focus stays with the item moved.
swapMaster :: StackSet i a s sd -> StackSet i a s sd
swapMaster = modify' $ \c -> case c of
    Stack _ [] _  -> c    -- already master.
    Stack t ls rs -> Stack t [] (ys ++ x : rs) where (x:ys) = reverse ls

-- natural! keep focus, move current to the top, move top to current.
-- 
-- ---------------------------------------------------------------------
-- $composite

-- | /O(w)/. shift. Move the focused element of the current stack to stack
-- 'n', leaving it as the focused element on that stack. The item is
-- inserted above the currently focused element on that workspace.  --
-- The actual focused workspace doesn't change. If there is -- no
-- element on the current stack, the original stackSet is returned.
--
shift :: (Ord a, Eq s, Integral i) => i -> StackSet i a s sd -> StackSet i a s sd
shift n s = if n `tagMember` s && n /= curtag
              then maybe s go (peek s) else s
    where go w = view curtag . insertUp w . view n . delete w $ s
          curtag = tag (workspace (current s))
