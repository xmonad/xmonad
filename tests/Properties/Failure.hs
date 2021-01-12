module Properties.Failure where

import XMonad.StackSet hiding (filter)

import qualified Control.Exception as C
import System.IO.Unsafe
import Data.List (isPrefixOf)

-- ---------------------------------------------------------------------
-- testing for failure and help out hpc
--
-- Since base 4.9.0.0 `error` appends a stack trace. The tests below
-- use `isPrefixOf` to only test equality on the error message.
--
prop_abort :: Int -> Bool
prop_abort _ = unsafePerformIO $ C.catch (abort "fail") check
   where
     check (C.SomeException e) =
       return $ "xmonad: StackSet: fail" `isPrefixOf` show e

-- new should fail with an abort
prop_new_abort :: Int -> Bool
prop_new_abort _ = unsafePerformIO $ C.catch f check
   where
     f = new undefined{-layout-} [] [] `seq` return False
     check (C.SomeException e) =
       return $ "xmonad: StackSet: non-positive argument to StackSet.new" `isPrefixOf` show e

-- TODO: Fix this?
-- prop_view_should_fail = view {- with some bogus data -}
