{-# OPTIONS -fglasgow-exts #-}

import StackSet
import Operations (tile)

import Debug.Trace
import Data.Word
import Graphics.X11.Xlib.Types (Rectangle(..),Position,Dimension)
import Data.Ratio
import Data.Maybe
import System.Environment
import Control.Exception    (assert)
import Control.Monad
import Test.QuickCheck hiding (promote)
import System.IO
import System.Random
import Text.Printf
import Data.List            (nub,sort,group,sort,intersperse,genericLength)
import Data.Char            (ord)
import Data.Map             (keys,elems)
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet


-- | fromList. Build a new StackSet from a list of list of elements,
-- keeping track of the currently focused workspace, and the total
-- number of workspaces. If there are duplicates in the list, the last
-- occurence wins.
fromList :: (Integral i, Integral j, Ord a) => (i, Int, [Maybe a], [[a]]) -> StackSet i j a
fromList (_,_,_,[]) = error "Cannot build a StackSet from an empty list"

fromList (n,m,fs,xs) | n < 0 || n >= genericLength xs
                = error $ "Cursor index is out of range: " ++ show (n, length xs)
                  | m < 1 || m >  genericLength xs
                = error $ "Can't have more screens than workspaces: " ++ show (m, length xs)

-- 'o' random workspace
-- 'fs' random focused window on each workspace
--
fromList (o,m,fs,xs) =
    let s = view o $
                foldr (\(i,ys) s ->
                    foldr (\a t -> insert a i t) s ys)
                        (empty (length xs) m) (zip [0..] xs)

    in foldr (\f s -> case f of
                            Nothing -> s
                            Just w  -> raiseFocus w s) s fs

-- ---------------------------------------------------------------------

-- | /O(n)/. Number of stacks
size :: T -> Int
size = M.size . stacks

-- | Height of stack 'n'
height :: Int -> T -> Int
height i w = maybe 0 length (index i w)

-- build (non-empty) StackSets with between 1 and 100 stacks
--
-- StackSet
--  { current :: i
--  , screen2ws:: !(M.Map j i)          -- ^ screen -> workspace
--  , ws2screen:: !(M.Map i j)          -- ^ workspace -> screen map
--  , stacks   :: !(M.Map i ([a], [a])) -- ^ screen -> (floating, normal)
--  , cache    :: !(M.Map a i)          -- ^ a cache of windows back to their stacks
--  }
--
-- Use 'raiseFocus' to bring focus to the front'
--
instance (Integral i, Integral j, Ord a, Arbitrary a) => Arbitrary (StackSet i j a) where
    arbitrary = do
        sz <- choose (1,20)
        n  <- choose (0,sz-1)
        sc <- choose (1,sz)
        ls <- vector sz

        -- pick a random element of each stack to focus.
        fs <- sequence [ if null s then return Nothing
                            else liftM Just (elements s)
                       | s <- ls ]

        return $ fromList (fromIntegral n,sc,fs,ls)
    coarbitrary = error "no coarbitrary for StackSet"

-- Invariants:
--
-- * no element should ever appear more than once in a StackSet
-- * the current index should always be valid
--
-- All operations must preserve this.
--
invariant (w :: T) = inBounds w && noDuplicates allWindows
    where
        allWindows      = concatMap (uncurry (++)) . M.elems . stacks  $ w
        noDuplicates ws = nub ws == ws
        inBounds x      = current x >= 0 && current x < sz where sz = M.size (stacks x)

-- test generator
prop_invariant = invariant


-- empty StackSets have no windows in them
prop_empty n m = n > 0 && m > 0 ==> all (null . uncurry (++)) (M.elems (stacks x))
    where x = empty n m :: T

-- empty StackSets always have focus on workspace 0
prop_empty_current n m = n > 0 && m > 0 ==> current x == 0
    where x = empty n m :: T

prop_member1 i n m = n > 0 && m > 0 ==> member i (push i x)
    where x = empty n m :: T

prop_member2 i x = not (member i (delete i x))
    where _ = x :: T

prop_member3 i n m = member i (empty n m :: T) == False

prop_sizepush is n m = n > 0 ==> size (foldr push x is ) == n
    where x  = empty n m :: T

prop_currentpush is n m = n > 0 ==>
    height (current x) (foldr push x js) == length js
    where
        js = nub is
        x = empty n m :: T

prop_push_idem i (x :: T) = push i x == push i (push i x)

prop_pushpeek x is = not (null is) ==> fromJust (peek (foldr push x is)) == head is
    where _ = x :: T

prop_peekmember x = case peek x of
                            Just w  -> member w x
                            Nothing -> True {- then we don't know anything -}
    where _ = x :: T

prop_peek_peekStack n x =
        if current x == n then peekStack n x == peek x
                          else True -- so we don't exhaust
    where _ = x :: T

prop_notpeek_peekStack n x = current x /= n && isJust (peek x) ==> peekStack n x /= peek x
    where _ = x :: T

------------------------------------------------------------------------

type T = StackSet Int Int Char

prop_delete_uniq i x = not (member i x) ==> delete i x == x
    where _ = x :: T

{-
TODO: enable this property when we have a better story about focus.

prop_delete_push i x = not (member i x) ==> delete i (push i x) == x
    where _ = x :: T
-}

prop_delete_push i x = not (member i x) ==> delete i (push i x) == x
    where _ = x :: T

prop_delete2 i x =
    delete i x == delete i (delete i x)
    where _ = x :: T

prop_focus1 i x = member i x ==> peek (raiseFocus i x) == Just i
    where _ = x :: T

-- rotation is reversible in two directions
prop_rotaterotate1 (x :: T)   = rotate LT (rotate GT x') == x'
    where x' = rotate LT x
prop_rotaterotate2 (x :: T)   = rotate GT (rotate LT x') == x'
    where x' = rotate GT x

-- rotation through the height of a stack gets us back to the start
prop_rotate_all (x :: T) = f (f x) == f x
  where
    n = height (current x) x
    f x' = foldr (\_ y -> rotate GT y) x' [1..n]


prop_viewview r  x   =
    let n  = current x
        sz = size x
        i  = r `mod` sz
    in view n (view (fromIntegral i) x) == x

    where _ = x :: T

prop_view_idem (x :: T) r =
    let i = fromIntegral $ r `mod` sz
        sz = size x
    in view i (view i x) == (view i x)

{-
TODO: enable this property when we have a better story for focus.

prop_shift_reversible r (x :: T) =
    let i  = fromIntegral $ r `mod` sz
        sz = size x
        n  = current x
    in height n x > 0 ==> (view n . shift n . view i . shift i) x == x
-}


prop_fullcache x = cached == allvals where
    cached  = sort . keys $ cache x
    allvals = sort . concat . map (uncurry (++)) . elems $ stacks x
    _       = x :: T

prop_currentwsvisible x = (current x) `elem` (visibleWorkspaces x)
    where _ = x :: T

prop_ws2screen_screen2ws x = (ws == ws') && (sc == sc')
    where ws  = sort . keys  $ ws2screen x
          ws' = sort . elems $ screen2ws x
          sc  = sort . keys  $ screen2ws x
          sc' = sort . elems $ ws2screen x
          _ = x :: T

prop_screenworkspace x = all test [0..((fromIntegral $ size x)-1)]
    where test ws = case screen ws x of
                        Nothing -> True
                        Just sc -> workspace sc x == Just ws
          _ = x :: T

prop_swap a b xs = swap a b (swap a b ys) == ys
    where ys = nub xs :: [Int]

------------------------------------------------------------------------

-- promote is idempotent
prop_promote2 x = promote (promote x) == (promote x)
  where _ = x :: T

-- focus doesn't change
prop_promotefocus x = focus (promote x) == focus x
  where _ = x :: T

-- screen certainly should't change
prop_promotecurrent x = current (promote x) == current x
  where _ = x :: T

-- the physical screen doesn't change
prop_promotescreen n x = screen n (promote x) == screen n x
  where _ = x :: T

-- promote doesn't mess with other windows
prop_promote_raise_id x = (not . null . fromMaybe [] . flip index x . current $ x) ==>
                          (promote . promote . promote) x == promote x
  where _            = x :: T

-- push shouldn't change anything but the current workspace
prop_push_local (x :: T) i = not (member i x) ==> hidden x == hidden (push i x)
  where
     hidden w = [ index n w | n <- [0 ..sz-1], n /= current w ]
     sz = M.size (stacks x)


------------------------------------------------------------------------
-- some properties for layouts:

-- 1 window should always be tiled fullscreen
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

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    results <- mapM (\(s,a) -> printf "%-25s: " s >> a n) tests
    when (not . and $ results) $ fail "Not all tests passed!"
 where
    n = 100

    tests =
        [("StackSet invariants", mytest prop_invariant)
        ,("empty is empty"   , mytest prop_empty)
        ,("empty / current"  , mytest prop_empty_current)

        ,("member/push      ", mytest prop_member1)
        ,("member/peek      ", mytest prop_peekmember)
        ,("member/delete    ", mytest prop_member2)
        ,("member/empty     ", mytest prop_member3)

        ,("size/push        ", mytest prop_sizepush)
        ,("height/push      ", mytest prop_currentpush)
        ,("push/peek        ", mytest prop_pushpeek)
        ,("push is local"    , mytest prop_push_local)
        ,("idempotent push" ,  mytest prop_push_idem)

        ,("peek/peekStack"  ,  mytest prop_peek_peekStack)
        ,("not . peek/peekStack", mytest prop_notpeek_peekStack)

        ,("delete/not.member", mytest prop_delete_uniq)
        ,("delete idempotent", mytest prop_delete2)
        ,("delete.push identity" , mytest prop_delete_push)

        ,("focus",             mytest prop_focus1)

        ,("rotate l >> rotate r", mytest prop_rotaterotate1)
        ,("rotate r >> rotate l", mytest prop_rotaterotate2)
        ,("rotate all", mytest prop_rotate_all)

        ,("view/view        ", mytest prop_viewview)
        ,("view idem        ", mytest prop_view_idem)

        -- disabled, for now ,("shift reversible ", mytest prop_shift_reversible)

        ,("fullcache        ", mytest prop_fullcache)
        ,("currentwsvisible ", mytest prop_currentwsvisible)
        ,("ws screen mapping", mytest prop_ws2screen_screen2ws)
        ,("screen/workspace ", mytest prop_screenworkspace)

        ,("promote idempotent", mytest prop_promote2)
        ,("promote focus",      mytest prop_promotefocus)
        ,("promote current",    mytest prop_promotecurrent)
        ,("promote only swaps", mytest prop_promote_raise_id)
        ,("promote/screen" ,    mytest prop_promotescreen)

        ,("swap",               mytest prop_swap)

------------------------------------------------------------------------

        ,("tile 1 window fullsize", mytest prop_tile_fullscreen)
        ,("tiles never overlap",    mytest prop_tile_non_overlap)

        ]

debug = False

mytest :: Testable a => a -> Int -> IO Bool
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO Bool
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO Bool
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = done "OK," ntest stamps >> return True
    | nfail == configMaxFail config = done "Arguments exhausted after" ntest stamps >> return True
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
                    ) >> hFlush stdout >> return False
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
