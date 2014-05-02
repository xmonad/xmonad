module Properties.Failure where

import XMonad.StackSet hiding (filter)

import qualified Control.Exception.Extensible as C
import System.IO.Unsafe

-- ---------------------------------------------------------------------
-- testing for failure

-- and help out hpc
prop_abort x = unsafePerformIO $ C.catch (abort "fail")
                                         (\(C.SomeException e) -> return $  show e == "xmonad: StackSet: fail" )
   where
     _ = x :: Int

-- new should fail with an abort
prop_new_abort x = unsafePerformIO $ C.catch f
                                         (\(C.SomeException e) -> return $ show e == "xmonad: StackSet: non-positive argument to StackSet.new" )
   where
     f = new undefined{-layout-} [] [] `seq` return False

     _ = x :: Int

-- TODO: Fix this?
-- prop_view_should_fail = view {- with some bogus data -}
