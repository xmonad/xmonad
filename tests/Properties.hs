{-# OPTIONS -fglasgow-exts #-}

import StackSet
import Operations (tile,vtile)

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
import Data.Map             (keys,elems)
import qualified Data.Map as M

-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet


-- | fromList. Build a new StackSet from a list of list of elements,
-- keeping track of the currently focused workspace, and the total
-- number of workspaces. If there are duplicates in the list, the last
-- occurence wins.
fromList :: (Integral i, Integral j, Ord a) => (i, Int,[[a]]) -> StackSet i j a
fromList (_,_,[]) = error "Cannot build a StackSet from an empty list"

fromList (n,m,xs) | n < 0 || n >= genericLength xs
                = error $ "Cursor index is out of range: " ++ show (n, length xs)
                  | m < 1 || m >  genericLength xs
                = error $ "Can't have more screens than workspaces: " ++ show (m, length xs)

fromList (o,m,xs) = view o $ foldr (\(i,ys) s ->
                                  foldr (\a t -> insert a i t) s ys)
                                      (empty (length xs) m) (zip [0..] xs)

-- ---------------------------------------------------------------------

-- | /O(n)/. Number of stacks
size :: T -> Int
size = M.size . stacks

-- | Height of stack 'n'
height :: Int -> T -> Int
height i w = length (index i w)

-- build (non-empty) StackSets with between 1 and 100 stacks
instance (Integral i, Integral j, Ord a, Arbitrary a) => Arbitrary (StackSet i j a) where
    arbitrary = do
        sz <- choose (1,20)
        n  <- choose (0,sz-1)
        sc <- choose (1,sz)
        ls <- vector sz
        return $ fromList (fromIntegral n,sc,ls)
    coarbitrary = error "no coarbitrary for StackSet"

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

type T = StackSet Int Int Int

prop_delete_uniq i x = not (member i x) ==> delete i x == x
    where _ = x :: T

prop_delete_push i x = not (member i x) ==> delete i (push i x) == x
    where _ = x :: T

prop_delete2 i x =
    delete i x == delete i (delete i x)
    where _ = x :: T

prop_focus1 i x = member i x ==> peek (raiseFocus i x) == Just i
    where _ = x :: T

prop_rotaterotate x   = rotate LT (rotate GT x) == x
    where _ = x :: T

prop_viewview r  x   =
    let n  = current x
        sz = size x
        i  = r `mod` sz
    in view n (view (fromIntegral i) x) == x

    where _ = x :: T

prop_shiftshift r x =
    let n  = current x
    in shift n (shift r x) == x
    where _ = x :: T

prop_fullcache x = cached == allvals where
    cached  = sort . keys $ cache x
    allvals = sort . concat . elems $ stacks x
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
prop_promoterotate x b = focus (rotate dir (promote x)) == focus (rotate dir x)
  where _ = x :: T
        dir = if b then LT else GT

------------------------------------------------------------------------
-- some properties for layouts:

-- 1 window should always be tiled fullscreen
prop_tile_fullscreen rect = tile pct rect [1] == [(1, rect)]

prop_vtile_fullscreen rect = vtile pct rect [1] == [(1, rect)]

-- multiple windows 
prop_tile_non_overlap rect windows = noOverlaps (tile pct rect windows)
  where _ = rect :: Rectangle

prop_vtile_non_overlap rect windows = noOverlaps (vtile pct rect windows)
  where _ = rect :: Rectangle

pct = 3 % 100

noOverlaps []  = True
noOverlaps [_] = True
noOverlaps xs  = and [ verts a `notOverlap` verts b
                     | (_,a) <- xs
                     , (_,b) <- filter (\(_,b) -> a /= b) xs
                     ]
    where
      verts (Rectangle a b w h) = (a,b,a + fromIntegral w - 1, b + fromIntegral h - 1)

      notOverlap (left1,bottom1,right1,top1)
                 (left2,bottom2,right2,top2)
        =  (top1 < bottom2 || top2 < bottom1)
        || (right1 < left2 || right2 < left1)


------------------------------------------------------------------------

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
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests
 where
    n = 100

    tests =
        [("member/push      ", mytest prop_member1)
        ,("member/peek      ", mytest prop_peekmember)
        ,("member/delete    ", mytest prop_member2)
        ,("member/empty     ", mytest prop_member3)

        ,("size/push        ", mytest prop_sizepush)
        ,("height/push      ", mytest prop_currentpush)
        ,("push/peek        ", mytest prop_pushpeek)

        ,("peek/peekStack"  ,  mytest prop_peek_peekStack)
        ,("not . peek/peekStack", mytest prop_notpeek_peekStack)

        ,("delete/not.member", mytest prop_delete_uniq)
        ,("delete idempotent", mytest prop_delete2)
        ,("delete.push identity" , mytest prop_delete_push)

        ,("focus",             mytest prop_focus1)

        ,("rotate/rotate    ", mytest prop_rotaterotate)

        ,("view/view        ", mytest prop_viewview)
        ,("fullcache        ", mytest prop_fullcache)
        ,("currentwsvisible ", mytest prop_currentwsvisible)
        ,("ws screen mapping", mytest prop_ws2screen_screen2ws)
        ,("screen/workspace ", mytest prop_screenworkspace)

        ,("promote idempotent", mytest prop_promote2)
        ,("promote focus",      mytest prop_promotefocus)
        ,("promote current",    mytest prop_promotecurrent)
        ,("promote only swaps", mytest prop_promoterotate)
        ,("promote/screen" ,    mytest prop_promotescreen)

        ,("swap",               mytest prop_swap)

------------------------------------------------------------------------

        ,("tile 1 window fullsize", mytest prop_tile_fullscreen)
        ,("vtile 1 window fullsize", mytest prop_vtile_fullscreen)
        ,("vtiles never overlap",    mytest prop_vtile_non_overlap     )

        ]

debug = False

mytest :: Testable a => a -> Int -> IO ()
mytest a n = mycheck defaultConfig
    { configMaxTest=n
    , configEvery= \n args -> if debug then show n ++ ":\n" ++ unlines args else [] } a

mycheck :: Testable a => Config -> a -> IO ()
mycheck config a = do
    rnd <- newStdGen
    mytests config (evaluate a) rnd 0 0 []

mytests :: Config -> Gen Result -> StdGen -> Int -> Int -> [[String]] -> IO ()
mytests config gen rnd0 ntest nfail stamps
    | ntest == configMaxTest config = do done "OK," ntest stamps
    | nfail == configMaxFail config = do done "Arguments exhausted after" ntest stamps
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
                    ) >> hFlush stdout
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
