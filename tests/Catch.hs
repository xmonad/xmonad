
-- This is a test set for running with Catch
-- http://www-users.cs.york.ac.uk/~ndm/catch/

module Catch where

import StackSet

---------------------------------------------------------------------
-- TESTING PROPERTIES

main = 
    screen ||| peekStack ||| index ||| empty ||| peek ||| push ||| delete ||| member |||
    raiseFocus ||| rotate ||| promote ||| shift ||| view ||| workspace ||| insert |||
    visibleWorkspaces ||| swap {- helper -}


---------------------------------------------------------------------
-- CATCH FIRST-ORDER LIBRARY

-- this should be included with Catch by default
-- and will be (one day!)

foreign import primitive any0 :: a
foreign import primitive anyEval1 :: a -> b
foreign import primitive anyEval2 :: a -> b -> c
foreign import primitive anyEval3 :: a -> b -> c -> d


class Test a where
    test :: a -> Bool


instance Test b => Test (a -> b) where
    test f = test (f any0)

instance Test (Maybe a) where
    test f = anyEval1 f

instance Test [a] where
    test f = anyEval1 f

instance Test (StackSet a b c) where
    test f = anyEval1 f

instance Test (a,b) where
    test f = anyEval1 f

instance Test Bool where
    test f = anyEval1 f

instance Test Char where
    test f = anyEval1 f

instance Test (IO a) where
    test f = anyEval1 (f >> return ())
    

(|||) :: (Test a, Test b) => a -> b -> IO c
(|||) l r = anyEval2 (test l) (test r)
