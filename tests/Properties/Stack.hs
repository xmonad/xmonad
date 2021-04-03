{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Stack where

import Test.QuickCheck
import Instances

import XMonad.StackSet hiding (filter)
import qualified XMonad.StackSet as S (filter)

import Data.Maybe

#ifdef VERSION_quickcheck_classes
import Data.Proxy
import Test.QuickCheck.Classes (
    Laws (lawsTypeclass, lawsProperties), Proxy1 (Proxy1),
    foldableLaws, traversableLaws,
    )
#endif


-- The list returned by index should be the same length as the actual
-- windows kept in the zipper
prop_index_length (x :: T) =
    case stack . workspace . current $ x of
        Nothing   -> length (index x) == 0
        Just it -> length (index x) == length (focus it : up it ++ down it)


-- For all windows in the stackSet, findTag should identify the
-- correct workspace
prop_findIndex (x :: T) =
    and [ tag w == fromJust (findTag i x)
        | w <- workspace (current x) : map workspace (visible x)  ++ hidden x
        , t <- maybeToList (stack w)
        , i <- focus t : up t ++ down t
        ]

prop_allWindowsMember (NonEmptyWindowsStackSet x) = do
      -- Reimplementation of arbitraryWindow, but to make sure that
      -- implementation doesn't change in the future, and stop using allWindows,
      -- which is a key component in this test (together with member).
  let ws = allWindows x
  -- We know that there are at least 1 window in a NonEmptyWindowsStackSet.
  idx <- choose(0, (length ws) - 1)
  return $ member (ws!!idx) x


-- preserve order
prop_filter_order (x :: T) =
    case stack $ workspace $ current x of
        Nothing -> True
        Just s@(Stack i _ _) -> integrate' (S.filter (/= i) s) == filter (/= i) (integrate' (Just s))

-- differentiate should return Nothing if the list is empty or Just stack, with
-- the first element of the list is current, and the rest of the list is down.
prop_differentiate xs =
        if null xs then differentiate xs == Nothing
                   else (differentiate xs) == Just (Stack (head xs) [] (tail xs))
    where _ = xs :: [Int]


#ifdef VERSION_quickcheck_classes
-- Check type class laws of 'Data.Foldable.Foldable' and 'Data.Traversable.Traversable'.
newtype TestStack a = TestStack (Stack a)
    deriving (Eq, Read, Show, Foldable, Functor)

instance (Arbitrary a) => Arbitrary (TestStack a) where
   arbitrary = TestStack <$> (Stack <$> arbitrary <*> arbitrary <*> arbitrary)
   shrink = traverse shrink

instance Traversable TestStack where
   traverse f (TestStack sx) = fmap TestStack (traverse f sx)

prop_laws_Stack = format (foldableLaws p) <> format (traversableLaws p)
  where
    p = Proxy :: Proxy TestStack
    format laws = [ ("Stack: " <> lawsTypeclass laws <> ": " <> name, prop)
                  | (name, prop) <- lawsProperties laws ]
#else
prop_laws_Stack = []
#endif
