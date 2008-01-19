{-# OPTIONS -fglasgow-exts -w #-}
module Properties where

import XMonad.StackSet hiding (filter)
import qualified XMonad.StackSet as S (filter)

import Debug.Trace
import Data.Word
import Graphics.X11.Xlib.Types (Rectangle(..),Position,Dimension)
import Data.Ratio
import Data.Maybe
import System.Environment
import Control.Exception    (assert)
import qualified Control.Exception as C
import Control.Monad
import Test.QuickCheck hiding (promote)
import System.IO.Unsafe
import System.IO
import System.Random hiding (next)
import Text.Printf
import Data.List            (nub,sort,sortBy,group,sort,intersperse,genericLength)
import qualified Data.List as L
import Data.Char            (ord)
import Data.Map             (keys,elems)
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet

-- Some general hints for creating StackSet properties:
--
-- *  ops that mutate the StackSet are usually local
-- *  most ops on StackSet should either be trivially reversible, or
--    idempotent, or both.

--
-- The all important Arbitrary instance for StackSet.
--
instance (Integral i, Integral s, Eq a, Arbitrary a, Arbitrary l, Arbitrary sd)
        => Arbitrary (StackSet i l a s sd) where
    arbitrary = do
        sz <- choose (1,10)     -- number of workspaces
        n  <- choose (0,sz-1)   -- pick one to be in focus
        sc  <- choose (1,sz)     -- a number of physical screens
        lay <- arbitrary           -- pick any layout
        sds <- replicateM sc arbitrary
        ls <- vector sz         -- a vector of sz workspaces

        -- pick a random item in each stack to focus
        fs <- sequence [ if null s then return Nothing
                            else liftM Just (choose ((-1),length s-1))
                       | s <- ls ]

        return $ fromList (fromIntegral n, sds,fs,ls,lay)


-- | fromList. Build a new StackSet from a list of list of elements,
-- keeping track of the currently focused workspace, and the total
-- number of workspaces. If there are duplicates in the list, the last
-- occurence wins.
--
-- 'o' random workspace
-- 'm' number of physical screens
-- 'fs' random focused window on each workspace
-- 'xs' list of list of windows
--
fromList :: (Integral i, Integral s, Eq a) => (i, [sd], [Maybe Int], [[a]], l) -> StackSet i l a s sd
fromList (_,_,_,[],_) = error "Cannot build a StackSet from an empty list"

fromList (o,m,fs,xs,l) =
    let s = view o $
                foldr (\(i,ys) s ->
                    foldr insertUp (view i s) ys)
                        (new l [0..genericLength xs-1] m) (zip [0..] xs)
    in foldr (\f t -> case f of
                            Nothing -> t
                            Just i  -> foldr (const focusUp) t [0..i] ) s fs

------------------------------------------------------------------------

--
-- Just generate StackSets with Char elements.
--
type T = StackSet (NonNegative Int) Int Char Int Int

-- Useful operation, the non-local workspaces
hidden_spaces x = map workspace (visible x) ++ hidden x

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
prop_empty_I  (n :: Positive Int) l = forAll (choose (1,fromIntegral n)) $  \m ->
                                      forAll (vector m) $ \ms ->
        invariant $ new l [0..fromIntegral n-1] ms

prop_view_I (n :: NonNegative Int) (x :: T) =
    n `tagMember` x ==> invariant $ view (fromIntegral n) x

prop_greedyView_I (n :: NonNegative Int) (x :: T) =
    n `tagMember` x ==> invariant $ view (fromIntegral n) x

prop_focusUp_I (n :: NonNegative Int) (x :: T) =
    invariant $ foldr (const focusUp) x [1..n]
prop_focusMaster_I (n :: NonNegative Int) (x :: T) =
    invariant $ foldr (const focusMaster) x [1..n]
prop_focusDown_I (n :: NonNegative Int) (x :: T) =
    invariant $ foldr (const focusDown) x [1..n]

prop_focus_I (n :: NonNegative Int) (x :: T) =
    case peek x of
        Nothing -> True
        Just _  -> let w = focus . fromJust . stack . workspace . current $ foldr (const focusUp) x [1..n]
                   in invariant $ focusWindow w x

prop_insertUp_I n (x :: T) = invariant $ insertUp n x

prop_delete_I (x :: T) = invariant $
    case peek x of
        Nothing -> x
        Just i  -> delete i x

prop_swap_master_I (x :: T) = invariant $ swapMaster x

prop_swap_left_I  (n :: NonNegative Int) (x :: T) =
    invariant $ foldr (const swapUp ) x [1..n]
prop_swap_right_I (n :: NonNegative Int) (x :: T) =
    invariant $ foldr (const swapDown) x [1..n]

prop_shift_I (n :: NonNegative Int) (x :: T) =
    n `tagMember` x ==> invariant $ shift (fromIntegral n) x

prop_shift_win_I (n :: NonNegative Int) (w :: Char) (x :: T) =
    n `tagMember` x && w `member` x ==> invariant $ shiftWin (fromIntegral n) w x


-- ---------------------------------------------------------------------
-- 'new'

-- empty StackSets have no windows in them
prop_empty (EmptyStackSet x) =
        all (== Nothing) [ stack w | w <- workspace (current x)
                                        : map workspace (visible x) ++ hidden x ]

-- empty StackSets always have focus on first workspace
prop_empty_current (NonEmptyNubList ns) (NonEmptyNubList sds) l =
    -- TODO, this is ugly
    length sds <= length ns ==>
    tag (workspace $ current x) == head ns
    where x = new l ns sds :: T

-- no windows will be a member of an empty workspace
prop_member_empty i (EmptyStackSet x)
    = member i x == False

-- ---------------------------------------------------------------------
-- viewing workspaces

-- view sets the current workspace to 'n'
prop_view_current (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
    tag (workspace $ current (view i x)) == i
  where
    i = fromIntegral n

-- view *only* sets the current workspace, and touches Xinerama.
-- no workspace contents will be changed.
prop_view_local  (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
    workspaces x == workspaces (view i x)
  where
    workspaces a = sortBy (\s t -> tag s `compare` tag t) $
                                    workspace (current a)
                                    : map workspace (visible a) ++ hidden a
    i = fromIntegral n

-- view should result in a visible xinerama screen
-- prop_view_xinerama (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
--     M.member i (screens (view i x))
--   where
--     i = fromIntegral n

-- view is idempotent
prop_view_idem (x :: T) (i :: NonNegative Int) = i `tagMember` x ==> view i (view i x) == (view i x)

-- view is reversible, though shuffles the order of hidden/visible
prop_view_reversible (i :: NonNegative Int) (x :: T) =
    i `tagMember` x ==> normal (view n (view i x)) == normal x
    where n  = tag (workspace $ current x)

-- ---------------------------------------------------------------------
-- greedyViewing workspaces

-- greedyView sets the current workspace to 'n'
prop_greedyView_current (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
    tag (workspace $ current (greedyView i x)) == i
  where
    i = fromIntegral n

-- greedyView *only* sets the current workspace, and touches Xinerama.
-- no workspace contents will be changed.
prop_greedyView_local  (x :: T) (n :: NonNegative Int) = i `tagMember` x ==>
    workspaces x == workspaces (greedyView i x)
  where
    workspaces a = sortBy (\s t -> tag s `compare` tag t) $
                                    workspace (current a)
                                    : map workspace (visible a) ++ hidden a
    i = fromIntegral n

-- greedyView is idempotent
prop_greedyView_idem (x :: T) (i :: NonNegative Int) = i `tagMember` x ==> greedyView i (greedyView i x) == (greedyView i x)

-- greedyView is reversible, though shuffles the order of hidden/visible
prop_greedyView_reversible (i :: NonNegative Int) (x :: T) =
    i `tagMember` x ==> normal (greedyView n (greedyView i x)) == normal x
    where n  = tag (workspace $ current x)

-- normalise workspace list
normal s = s { hidden = sortBy g (hidden s), visible = sortBy f (visible s) }
    where
        f = \a b -> tag (workspace a) `compare` tag (workspace b)
        g = \a b -> tag a `compare` tag b

-- ---------------------------------------------------------------------
-- Xinerama

-- every screen should yield a valid workspace
-- prop_lookupWorkspace (n :: NonNegative Int) (x :: T) =
--       s < M.size (screens x) ==>
--       fromJust (lookupWorkspace s x) `elem` (map tag $ current x : prev x ++ next x)
--     where
--        s = fromIntegral n

-- ---------------------------------------------------------------------
-- peek/index

-- peek either yields nothing on the Empty workspace, or Just a valid window
prop_member_peek (x :: T) =
    case peek x of
        Nothing -> True {- then we don't know anything -}
        Just i  -> member i x

-- ---------------------------------------------------------------------
-- index

-- the list returned by index should be the same length as the actual
-- windows kept in the zipper
prop_index_length (x :: T) =
    case stack . workspace . current $ x of
        Nothing   -> length (index x) == 0
        Just it -> length (index x) == length (focus it : up it ++ down it)

-- ---------------------------------------------------------------------
-- rotating focus
--

-- master/focus
--
-- The tiling order, and master window, of a stack is unaffected by focus changes.
--
prop_focus_left_master (n :: NonNegative Int) (x::T) =
    index (foldr (const focusUp) x [1..n]) == index x
prop_focus_right_master (n :: NonNegative Int) (x::T) =
    index (foldr (const focusDown) x [1..n]) == index x
prop_focus_master_master (n :: NonNegative Int) (x::T) =
    index (foldr (const focusMaster) x [1..n]) == index x

prop_focusWindow_master (n :: NonNegative Int) (x :: T) =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = fromIntegral n `mod` length s
                   in index (focusWindow (s !! i) x) == index x

-- shifting focus is trivially reversible
prop_focus_left  (x :: T) = (focusUp  (focusDown x)) == x
prop_focus_right (x :: T) = (focusDown (focusUp  x)) ==  x

-- focus master is idempotent
prop_focusMaster_idem (x :: T) = focusMaster x == focusMaster (focusMaster x)

-- focusWindow actually leaves the window focused...
prop_focusWindow_works (n :: NonNegative Int) (x :: T) =
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

prop_focusWindow_local (n :: NonNegative Int) (x::T ) =
    case peek x of
        Nothing -> True
        Just _  -> let s = index x
                       i = fromIntegral n `mod` length s
                   in hidden_spaces (focusWindow (s !! i) x) == hidden_spaces x

-- ---------------------------------------------------------------------
-- member/findTag

--
-- For all windows in the stackSet, findTag should identify the
-- correct workspace
--
prop_findIndex (x :: T) =
    and [ tag w == fromJust (findTag i x)
        | w <- workspace (current x) : map workspace (visible x)  ++ hidden x
        , t <- maybeToList (stack w)
        , i <- focus t : up t ++ down t
        ]

prop_allWindowsMember w (x :: T) = (w `elem` allWindows x) ==> member w x

-- ---------------------------------------------------------------------
-- 'insert'

-- inserting a item into an empty stackset means that item is now a member
prop_insert_empty i (EmptyStackSet x)= member i (insertUp i x)

-- insert should be idempotent
prop_insert_idem i (x :: T) = insertUp i x == insertUp i (insertUp i x)

-- insert when an item is a member should leave the stackset unchanged
prop_insert_duplicate i (x :: T) = member i x ==> insertUp i x == x

-- push shouldn't change anything but the current workspace
prop_insert_local (x :: T) i = not (member i x) ==> hidden_spaces x == hidden_spaces (insertUp i x)

-- Inserting a (unique) list of items into an empty stackset should
-- result in the last inserted element having focus.
prop_insert_peek (EmptyStackSet x) (NonEmptyNubList is) =
    peek (foldr insertUp x is) == Just (head is)

-- insert >> delete is the identity, when i `notElem` .
-- Except for the 'master', which is reset on insert and delete.
--
prop_insert_delete n x = not (member n x) ==> delete n (insertUp n y) == (y :: T)
    where
        y = swapMaster x -- sets the master window to the current focus.
                         -- otherwise, we don't have a rule for where master goes.

-- inserting n elements increases current stack size by n
prop_size_insert is (EmptyStackSet x) =
        size (foldr insertUp x ws ) ==  (length ws)
  where
    ws   = nub is
    size = length . index


-- ---------------------------------------------------------------------
-- 'delete'

-- deleting the current item removes it.
prop_delete x =
    case peek x of
        Nothing -> True
        Just i  -> not (member i (delete i x))
    where _ = x :: T

-- delete is reversible with 'insert'.
-- It is the identiy, except for the 'master', which is reset on insert and delete.
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
prop_delete_focus n (x :: T) = member n x && Just n /= peek x ==> peek (delete n x) == peek x

-- focus movement in the presence of delete:
-- when the last window in the stack set is focused, focus moves `up'.
-- usual case is that it moves 'down'.
prop_delete_focus_end (x :: T) =
    length (index x) > 1
   ==>
    peek (delete n y) == peek (focusUp y)
  where
    n = last (index x)
    y = focusWindow n x -- focus last window in stack

-- focus movement in the presence of delete:
-- when not in the last item in the stack, focus moves down
prop_delete_focus_not_end (x :: T) =
    length (index x) > 1 &&
    n /= last (index x)
   ==>
    peek (delete n x) == peek (focusDown x)
  where
    Just n = peek x

-- ---------------------------------------------------------------------
-- filter

-- preserve order
prop_filter_order (x :: T) =
    case stack $ workspace $ current x of
        Nothing -> True
        Just s@(Stack i _ _) -> integrate' (S.filter (/= i) s) == filter (/= i) (integrate' (Just s))

-- ---------------------------------------------------------------------
-- swapUp, swapDown, swapMaster: reordiring windows

-- swap is trivially reversible
prop_swap_left  (x :: T) = (swapUp  (swapDown x)) == x
prop_swap_right (x :: T) = (swapDown (swapUp  x)) ==  x
-- TODO swap is reversible
-- swap is reversible, but involves moving focus back the window with
-- master on it. easy to do with a mouse...
{-
prop_promote_reversible x b = (not . null . fromMaybe [] . flip index x . current $ x) ==>
                            (raiseFocus y . promote . raiseFocus z . promote) x == x
  where _            = x :: T
        dir          = if b then LT else GT
        (Just y)     = peek x
        (Just (z:_)) = flip index x . current $ x
-}

-- swap doesn't change focus
prop_swap_master_focus (x :: T) = peek x == (peek $ swapMaster x)
--    = case peek x of
--        Nothing -> True
--        Just f  -> focus (stack (workspace $ current (swap x))) == f
prop_swap_left_focus   (x :: T) = peek x == (peek $ swapUp   x)
prop_swap_right_focus  (x :: T) = peek x == (peek $ swapDown  x)

-- swap is local
prop_swap_master_local (x :: T) = hidden_spaces x == hidden_spaces (swapMaster x)
prop_swap_left_local   (x :: T) = hidden_spaces x == hidden_spaces (swapUp   x)
prop_swap_right_local  (x :: T) = hidden_spaces x == hidden_spaces (swapDown  x)

-- rotation through the height of a stack gets us back to the start
prop_swap_all_l (x :: T) = (foldr (const swapUp)  x [1..n]) == x
  where n = length (index x)
prop_swap_all_r (x :: T) = (foldr (const swapDown) x [1..n]) == x
  where n = length (index x)

prop_swap_master_idempotent (x :: T) = swapMaster (swapMaster x) == swapMaster x

-- ---------------------------------------------------------------------
-- shift

-- shift is fully reversible on current window, when focus and master
-- are the same. otherwise, master may move.
prop_shift_reversible i (x :: T) =
    i `tagMember` x ==> case peek y of
                          Nothing -> True
                          Just _  -> normal ((view n . shift n . view i . shift i) y) == normal y
    where
        y = swapMaster x
        n = tag (workspace $ current y)

-- ---------------------------------------------------------------------
-- shiftWin

-- shiftWin on current window is the same as shift
prop_shift_win_focus i (x :: T) =
    i `tagMember` x ==> case peek x of
                          Nothing -> True
                          Just w  -> shiftWin i w x == shift i x

-- shiftWin on a non-existant window is identity
prop_shift_win_indentity i w (x :: T) =
    i `tagMember` x && not (w  `member` x) ==> shiftWin i w x == x

-- shiftWin leaves the current screen as it is, if neither i is the tag
-- of the current workspace nor w on the current workspace
prop_shift_win_fix_current i w (x :: T) =
    i `tagMember` x && w `member` x && i /= n && findTag w x /= Just n
        ==> (current $ x) == (current $ shiftWin i w x)
    where
        n = tag (workspace $ current x)

------------------------------------------------------------------------
-- properties for the floating layer:

prop_float_reversible n (x :: T) =
    n `member` x ==> sink n (float n geom x) == x
        where
            geom = RationalRect 100 100 100 100

-- check rectanges were set
{-
prop_float_sets_geometry n (x :: T) =
    n `member` x ==> let y = float n geom x in M.lookup y (floating x) == Just geom
        where
            geom = RationalRect 100 100 100 100
-}

------------------------------------------------------------------------

prop_screens (x :: T) = n `elem` screens x
 where
    n = current x

prop_differentiate xs =
        if null xs then differentiate xs == Nothing
                   else (differentiate xs) == Just (Stack (head xs) [] (tail xs))
    where _ = xs :: [Int]

-- looking up the tag of the current workspace should always produce a tag.
prop_lookup_current (x :: T) = lookupWorkspace scr x == Just tg
    where
        (Screen (Workspace tg  _ _) scr _) = current x

-- looking at a visible tag
prop_lookup_visible (x :: T) =
        visible x /= [] ==>
        fromJust (lookupWorkspace scr x) `elem` tags
    where
        tags = [ tag (workspace y) | y <- visible x ]
        scr = last [ screen y | y <- visible x ]


-- ---------------------------------------------------------------------
-- testing for failure

-- and help out hpc
prop_abort x = unsafePerformIO $ C.catch (abort "fail")
                                         (\e -> return $  show e == "xmonad: StackSet: fail" )
   where
     _ = x :: Int

-- new should fail with an abort
prop_new_abort x = unsafePerformIO $ C.catch f
                                         (\e -> return $ show e == "xmonad: StackSet: non-positive argument to StackSet.new" )
   where
     f = new undefined{-layout-} [] [] `seq` return False

     _ = x :: Int

-- prop_view_should_fail = view {- with some bogus data -}

-- screens makes sense
prop_screens_works (x :: T) = screens x == current x : visible x

------------------------------------------------------------------------
-- renaming tags

-- | Rename a given tag if present in the StackSet.
-- 408 renameTag :: Eq i => i -> i -> StackSet i l a s sd -> StackSet i l a s sd

prop_rename1 (x::T) o n = o `tagMember` x && not (n `tagMember` x) ==>
    let y = renameTag o n x
            in n `tagMember` y

prop_ensure (x :: T) l xs = let y = ensureTags l xs x
                                in and [ n `tagMember` y | n <- xs ]

prop_mapWorkspaceId (x::T) = x == mapWorkspace id x

prop_mapWorkspaceInverse (x::T) = x == mapWorkspace predTag (mapWorkspace succTag x)
  where predTag w = w { tag = pred $ tag w }
        succTag w = w { tag = succ $ tag w }

prop_mapLayoutId (x::T) = x == mapLayout id x

prop_mapLayoutInverse (x::T) = x == mapLayout pred (mapLayout succ x)

------------------------------------------------------------------------
-- some properties for layouts:

-- 1 window should always be tiled fullscreen
{-
prop_tile_fullscreen rect = tile pct rect 1 1 == [rect]

-- multiple windows
prop_tile_non_overlap rect windows nmaster = noOverlaps (tile pct rect nmaster windows)
  where _ = rect :: Rectangle

pct = 3 % 100

noOverlaps []  = True
noOverlaps [_] = True
noOverlaps xs  = and [ verts a `notOverlap` verts b
                     | a <- xs
                     , b <- filter (a /=) xs
                     ]
    where
      verts (Rectangle a b w h) = (a,b,a + fromIntegral w - 1, b + fromIntegral h - 1)

      notOverlap (left1,bottom1,right1,top1)
                 (left2,bottom2,right2,top2)
        =  (top1 < bottom2 || top2 < bottom1)
        || (right1 < left2 || right2 < left1)

-}

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- fmap (drop 1) getArgs
    let n = if null args then 100 else read (head args)
    (results, passed) <- liftM unzip $ mapM (\(s,a) -> printf "%-25s: " s >> a n) tests
    printf "Passed %d tests!\n" (sum passed)
    when (not . and $ results) $ fail "Not all tests passed!"
 where

    tests =
        [("StackSet invariants" , mytest prop_invariant)

        ,("empty: invariant"    , mytest prop_empty_I)
        ,("empty is empty"      , mytest prop_empty)
        ,("empty / current"     , mytest prop_empty_current)
        ,("empty / member"      , mytest prop_member_empty)

        ,("view : invariant"    , mytest prop_view_I)
        ,("view sets current"   , mytest prop_view_current)
        ,("view idempotent"     , mytest prop_view_idem)
        ,("view reversible"    , mytest prop_view_reversible)
--      ,("view / xinerama"     , mytest prop_view_xinerama)
        ,("view is local"       , mytest prop_view_local)

        ,("greedyView : invariant"    , mytest prop_greedyView_I)
        ,("greedyView sets current"   , mytest prop_greedyView_current)
        ,("greedyView idempotent"     , mytest prop_greedyView_idem)
        ,("greedyView reversible"     , mytest prop_greedyView_reversible)
        ,("greedyView is local"       , mytest prop_greedyView_local)
--
--      ,("valid workspace xinerama", mytest prop_lookupWorkspace)

        ,("peek/member "        , mytest prop_member_peek)

        ,("index/length"        , mytest prop_index_length)

        ,("focus left : invariant", mytest prop_focusUp_I)
        ,("focus master : invariant", mytest prop_focusMaster_I)
        ,("focus right: invariant", mytest prop_focusDown_I)
        ,("focusWindow: invariant", mytest prop_focus_I)
        ,("focus left/master"   , mytest prop_focus_left_master)
        ,("focus right/master"  , mytest prop_focus_right_master)
        ,("focus master/master"  , mytest prop_focus_master_master)
        ,("focusWindow master"  , mytest prop_focusWindow_master)
        ,("focus left/right"    , mytest prop_focus_left)
        ,("focus right/left"    , mytest prop_focus_right)
        ,("focus all left  "    , mytest prop_focus_all_l)
        ,("focus all right "    , mytest prop_focus_all_r)
        ,("focus down is local"      , mytest prop_focus_down_local)
        ,("focus up is local"      , mytest prop_focus_up_local)
        ,("focus master is local"      , mytest prop_focus_master_local)
        ,("focus master idemp"  , mytest prop_focusMaster_idem)

        ,("focusWindow is local", mytest prop_focusWindow_local)
        ,("focusWindow works"   , mytest prop_focusWindow_works)

        ,("findTag"           , mytest prop_findIndex)
        ,("allWindows/member"   , mytest prop_allWindowsMember)

        ,("insert: invariant"   , mytest prop_insertUp_I)
        ,("insert/new"          , mytest prop_insert_empty)
        ,("insert is idempotent", mytest prop_insert_idem)
        ,("insert is reversible", mytest prop_insert_delete)
        ,("insert is local"     , mytest prop_insert_local)
        ,("insert duplicates"   , mytest prop_insert_duplicate)
        ,("insert/peek "        , mytest prop_insert_peek)
        ,("insert/size"         , mytest prop_size_insert)

        ,("delete: invariant"   , mytest prop_delete_I)
        ,("delete/empty"        , mytest prop_empty)
        ,("delete/member"       , mytest prop_delete)
        ,("delete is reversible", mytest prop_delete_insert)
        ,("delete is local"     , mytest prop_delete_local)
        ,("delete/focus"        , mytest prop_delete_focus)
        ,("delete  last/focus up", mytest prop_delete_focus_end)
        ,("delete ~last/focus down", mytest prop_delete_focus_not_end)

        ,("filter preserves order", mytest prop_filter_order)

        ,("swapMaster: invariant", mytest prop_swap_master_I)
        ,("swapUp: invariant" , mytest prop_swap_left_I)
        ,("swapDown: invariant", mytest prop_swap_right_I)
        ,("swapMaster id on focus", mytest prop_swap_master_focus)
        ,("swapUp id on focus", mytest prop_swap_left_focus)
        ,("swapDown id on focus", mytest prop_swap_right_focus)
        ,("swapMaster is idempotent", mytest prop_swap_master_idempotent)
        ,("swap all left  "     , mytest prop_swap_all_l)
        ,("swap all right "     , mytest prop_swap_all_r)
        ,("swapMaster is local" , mytest prop_swap_master_local)
        ,("swapUp is local"   , mytest prop_swap_left_local)
        ,("swapDown is local"  , mytest prop_swap_right_local)

        ,("shift: invariant"    , mytest prop_shift_I)
        ,("shift is reversible" , mytest prop_shift_reversible)
        ,("shiftWin: invariant" , mytest prop_shift_win_I)
        ,("shiftWin is shift on focus" , mytest prop_shift_win_focus)
        ,("shiftWin fix current" , mytest prop_shift_win_fix_current)

        ,("floating is reversible" , mytest prop_float_reversible)
        ,("screens includes current", mytest prop_screens)
        ,("differentiate works", mytest prop_differentiate)
        ,("lookupTagOnScreen", mytest prop_lookup_current)
        ,("lookupTagOnVisbleScreen", mytest prop_lookup_visible)
        ,("screens works",      mytest prop_screens_works)
        ,("renaming works",     mytest prop_rename1)
        ,("ensure works",     mytest prop_ensure)

        ,("mapWorkspace id", mytest prop_mapWorkspaceId)
        ,("mapWorkspace inverse", mytest prop_mapWorkspaceInverse)
        ,("mapLayout id", mytest prop_mapLayoutId)
        ,("mapLayout inverse", mytest prop_mapLayoutInverse)

        -- testing for failure:
        ,("abort fails",            mytest prop_abort)
        ,("new fails with abort",   mytest prop_new_abort)
        ,("shiftWin identity",      mytest prop_shift_win_indentity)

        -- renaming

{-
        ,("tile 1 window fullsize", mytest prop_tile_fullscreen)
        ,("tiles never overlap",    mytest prop_tile_non_overlap)
-}

        ]

------------------------------------------------------------------------
--
-- QC driver
--

debug = False

mytest :: Testable a => a -> Int -> IO (Bool, Int)
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery   = \n args -> let s = show n in s ++ [ '\b' | _ <- s ] } a
 -- , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO (Bool, Int)
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO (Bool, Int)
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return (True, ntest)
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return (True, ntest)
    | otherwise               =
      do putStr (configEvery config ntest (arguments result)) >> hFlush stdout
         case ok result of
           Nothing    ->
             mytests config gen rnd1 ntest (nfail+1) stamps
           Just True  ->
             mytests config gen rnd1 (ntest+1) nfail (stamp result:stamps)
           Just False ->
             putStr ( "Falsifiable after "
                   ++ show ntest
                   ++ " tests:\n"
                   ++ unlines (arguments result)
                    ) >> hFlush stdout >> return (False, ntest)
     where
      result      = generate (configSize config ntest) rnd2 gen
      (rnd1,rnd2) = split rnd0

done :: String -> Int -> [[String]] -> IO ()
done mesg ntest stamps = putStr ( mesg ++ " " ++ show ntest ++ " tests" ++ table )
  where
    table = display
            . map entry
            . reverse
            . sort
            . map pairLength
            . group
            . sort
            . filter (not . null)
            $ stamps

    display []  = ".\n"
    display [x] = " (" ++ x ++ ").\n"
    display xs  = ".\n" ++ unlines (map (++ ".") xs)

    pairLength xss@(xs:_) = (length xss, xs)
    entry (n, xs)         = percentage n ntest
                       ++ " "
                       ++ concat (intersperse ", " xs)

    percentage n m        = show ((100 * n) `div` m) ++ "%"

------------------------------------------------------------------------

instance Arbitrary Char where
    arbitrary = choose ('a','z')
    coarbitrary n = coarbitrary (ord n)

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word8 where
  arbitrary     = choose (minBound,maxBound)
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Arbitrary Word64 where
  arbitrary     = choose (minBound,maxBound)
  coarbitrary n = variant (fromIntegral ((fromIntegral n) `rem` 4))

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

instance Arbitrary Position  where
    arbitrary = do n <- arbitrary :: Gen Word8
                   return (fromIntegral n)
    coarbitrary = undefined

instance Arbitrary Dimension where
    arbitrary = do n <- arbitrary :: Gen Word8
                   return (fromIntegral n)
    coarbitrary = undefined

instance Arbitrary Rectangle where
    arbitrary = do
        sx <- arbitrary
        sy <- arbitrary
        sw <- arbitrary
        sh <- arbitrary
        return $ Rectangle sx sy sw sh
    coarbitrary = undefined

instance Arbitrary Rational where
    arbitrary = do
        n <- arbitrary
        d' <- arbitrary
        let d =  if d' == 0 then 1 else d'
        return (n % d)
    coarbitrary = undefined

------------------------------------------------------------------------
-- QC 2

-- from QC2
-- | NonEmpty xs: guarantees that xs is non-empty.
newtype NonEmptyList a = NonEmpty [a]
 deriving ( Eq, Ord, Show, Read )

instance Arbitrary a => Arbitrary (NonEmptyList a) where
  arbitrary   = NonEmpty `fmap` (arbitrary `suchThat` (not . null))
  coarbitrary = undefined

newtype NonEmptyNubList a = NonEmptyNubList [a]
 deriving ( Eq, Ord, Show, Read )

instance (Eq a, Arbitrary a) => Arbitrary (NonEmptyNubList a) where
  arbitrary   = NonEmptyNubList `fmap` ((liftM nub arbitrary) `suchThat` (not . null))
  coarbitrary = undefined

type Positive a = NonZero (NonNegative a)

newtype NonZero a = NonZero a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonZero a) where
  arbitrary = fmap NonZero $ arbitrary `suchThat` (/= 0)
  coarbitrary = undefined

newtype NonNegative a = NonNegative a
 deriving ( Eq, Ord, Num, Integral, Real, Enum, Show, Read )

instance (Num a, Ord a, Arbitrary a) => Arbitrary (NonNegative a) where
  arbitrary =
    frequency
      [ (5, (NonNegative . abs) `fmap` arbitrary)
      , (1, return 0)
      ]
  coarbitrary = undefined

newtype EmptyStackSet = EmptyStackSet T deriving Show

instance Arbitrary EmptyStackSet where
    arbitrary = do
        (NonEmptyNubList ns)  <- arbitrary
        (NonEmptyNubList sds) <- arbitrary
        l <- arbitrary
        -- there cannot be more screens than workspaces:
        return . EmptyStackSet . new l ns $ take (min (length ns) (length sds)) sds
    coarbitrary = error "coarbitrary EmptyStackSet"

-- | Generates a value that satisfies a predicate.
suchThat :: Gen a -> (a -> Bool) -> Gen a
gen `suchThat` p =
  do mx <- gen `suchThatMaybe` p
     case mx of
       Just x  -> return x
       Nothing -> sized (\n -> resize (n+1) (gen `suchThat` p))

-- | Tries to generate a value that satisfies a predicate.
suchThatMaybe :: Gen a -> (a -> Bool) -> Gen (Maybe a)
gen `suchThatMaybe` p = sized (try 0 . max 1)
 where
  try _ 0 = return Nothing
  try k n = do x <- resize (2*k+n) gen
               if p x then return (Just x) else try (k+1) (n-1)
