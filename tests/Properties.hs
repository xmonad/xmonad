
import StackSet

import Data.Maybe
import System.Environment
import Control.Exception    (assert)
import Control.Monad
import Test.QuickCheck
import System.IO
import System.Random
import Text.Printf
import Data.List            (nub,sort,group,sort,intersperse)
import Data.Map             (keys,elems)

-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet

-- | Height of stack 'n'
height :: Int -> StackSet a -> Int
height i w = length (index i w)

-- build (non-empty) StackSets with between 1 and 100 stacks
instance (Ord a, Arbitrary a) => Arbitrary (StackSet a) where
    arbitrary = do
        sz <- choose (1,20)
        n  <- choose (0,sz-1)
        ls <- vector sz
        return $ fromList (n,ls)
    coarbitrary = error "no coarbitrary for StackSet"

prop_id x = fromList (toList x) == x
    where _ = x :: T

prop_member1 i n m = member i (push i x)
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

type T = StackSet Int

prop_delete_uniq i x = not (member i x) ==> delete i x == x
    where _ = x :: T

prop_delete2 i x =
    delete i x == delete i (delete i x)
    where _ = x :: T

prop_rotaterotate x   = rotate LT (rotate GT x) == x
    where _ = x :: T

prop_viewview r  x   =
    let n  = current x
        sz = size x
        i  = r `mod` sz
    in view n (view i x) == x

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
                          
------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests
 where
    n = 100

    tests =
        [("read.show        ", mytest prop_id)
        ,("member/push      ", mytest prop_member1)
        ,("member/peek      ", mytest prop_peekmember)
        ,("member/delete    ", mytest prop_member2)
        ,("member/empty     ", mytest prop_member3)
        ,("size/push        ", mytest prop_sizepush)
        ,("height/push      ", mytest prop_currentpush)
        ,("push/peek        ", mytest prop_pushpeek)
        ,("delete/not.member", mytest prop_delete_uniq)
        ,("delete idempotent", mytest prop_delete2)
        ,("rotate/rotate    ", mytest prop_rotaterotate)
        ,("view/view        ", mytest prop_viewview)
        ,("fullcache        ", mytest prop_fullcache)
        ,("currentwsvisible ", mytest prop_currentwsvisible)
        ,("ws screen mapping", mytest prop_ws2screen_screen2ws)
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
