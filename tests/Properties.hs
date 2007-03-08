
import StackSet

import System.Environment
import Control.Exception    (assert)
import Control.Monad
import Test.QuickCheck
import System.IO
import System.Random
import Text.Printf
import Data.List            (sort,group,sort,intersperse)

-- ---------------------------------------------------------------------
-- QuickCheck properties for the StackSet

-- | Height of stack 'n'
height :: Int -> StackSet a -> Int
height i w = case index w i of
                Nothing -> error $ "height: i out of range: " ++ show i
                Just ss -> length ss

-- build (non-empty) StackSets with between 1 and 100 stacks
instance (Ord a, Arbitrary a) => Arbitrary (StackSet a) where
    arbitrary = do
        sz <- choose (1,20)
        n  <- choose (0,sz-1)
        ls <- vector sz
        return $ fromList (n,ls)
    coarbitrary = error "no coarbitrary for StackSet"

prop_id x = fromList (toList x) == x
    where _ = x :: StackSet Int

prop_uniq_pushpop i x = not (member i x) ==>
    (pop . push i) x == x
    where _ = x :: StackSet Int

prop_pushpop i x =
    (pop . push i) x == delete i x
    where _ = x :: StackSet Int

-- popping an empty stack leaves an empty stack
prop_popempty x = height (cursor x) x == 0 ==> pop x == x
    where _ = x :: StackSet Int

prop_popone   x =
    let l = height (cursor x) x
    in l > 0 ==> height (cursor x) (pop x) == l-1
    where _ = x :: StackSet Int

prop_delete_uniq i x = not (member i x) ==>
    delete i x == x
    where _ = x :: StackSet Int

prop_delete2 i x =
    delete i x == delete i (delete i x)
    where _ = x :: StackSet Int

prop_uniq_insertdelete i n x = not (member i x) ==>
    delete i (insert i n x) == x
    where _ = x :: StackSet Int

prop_insertdelete i n x =
    delete i (insert i n x) == delete i x
    where _ = x :: StackSet Int

prop_rotaterotate x   = rotate LT (rotate GT x) == x
    where _ = x :: StackSet Int

prop_viewview r  x   =
    let n  = cursor x
        sz = size x
        i  = r `mod` sz
    in
        view n (view i x) == x

    where _ = x :: StackSet Int

prop_shiftshift r x =
    let n  = cursor x
    in
        shift n (shift r x) == x
    where _ = x :: StackSet Int

------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getArgs
    let n = if null args then 100 else read (head args)
    mapM_ (\(s,a) -> printf "%-25s: " s >> a n) tests
 where
    n = 100

    tests =
        [("fromList.toList  ", mytest prop_id)
        ,("pop/push         ", mytest prop_uniq_pushpop)
        ,("pop/push/delete  ", mytest prop_pushpop)
        ,("pop/empty        ", mytest prop_popempty)
        ,("delete/not.member", mytest prop_delete_uniq)
        ,("delete idempotent", mytest prop_delete2)
        ,("delete/insert new", mytest prop_uniq_insertdelete)
        ,("delete/insert    ", mytest prop_insertdelete)
        ,("rotate/rotate    ", mytest prop_rotaterotate)
        ,("pop one          ", mytest prop_popone)
        ,("view/view        ", mytest prop_viewview)
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
