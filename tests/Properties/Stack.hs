{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Properties.Stack where

import Test.QuickCheck
import Test.QuickCheck.Classes (
    Laws (lawsTypeclass, lawsProperties), Proxy1 (Proxy1),
    foldableLaws, traversableLaws,
    )
import Instances

import XMonad.StackSet hiding (filter)
import qualified XMonad.StackSet as S (filter)

import Data.Maybe
import Data.Functor.Classes (Eq1 (liftEq), Show1 (liftShowsPrec))


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


-- Check type class laws of 'Data.Foldable.Foldable' and 'Data.Traversable.Traversable'.
newtype TestStack a = TestStack (Stack a)
    deriving (Eq, Read, Show, Foldable, Functor)

instance Arbitrary1 TestStack where
   liftArbitrary gen =
       (\ x xu xd -> TestStack (Stack x xu xd))
       <$> gen
       <*> liftArbitrary gen
       <*> liftArbitrary gen

instance (Arbitrary a)=> Arbitrary (TestStack a) where
   arbitrary = arbitrary1
   shrink = shrink1

instance Traversable TestStack where
   traverse f (TestStack sx) = fmap TestStack (traverse f sx)

instance Eq1 TestStack where
    liftEq f (TestStack (Stack x xu xd)) (TestStack (Stack y yu yd)) =
        f x y && liftEq f xu yu && liftEq f xd yd

instance Show1 TestStack where
    liftShowsPrec shwP shwL p (TestStack (Stack x xu xd)) =
        showString "TestStack (Stack {focus = "
                <> shwP p x
                <> showString ", up = "
                <> shwL xu
                <> showString ", down ="
                <> shwL xd
                <> showString "})"

proxy_TestStack :: Proxy1 TestStack
proxy_TestStack = Proxy1
laws_Stack_Traversable, laws_Stack_Foldable :: Laws
laws_Stack_Traversable = traversableLaws proxy_TestStack
laws_Stack_Foldable = foldableLaws proxy_TestStack
prop_laws_Stack =
    format laws_Stack_Foldable <> format laws_Stack_Traversable
  where
    format laws =
        fmap
        (\(name, prop) ->
            ("Stack: " <> lawsTypeclass laws <> ": " <> name, prop))
        (lawsProperties laws)
