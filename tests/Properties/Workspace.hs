{-# LANGUAGE ScopedTypeVariables #-}
module Properties.Workspace where

import Test.QuickCheck
import Instances
import Utils

import XMonad.StackSet hiding (filter)

import Data.Maybe

-- looking up the tag of the current workspace should always produce a tag.
prop_lookup_current (x :: T) = lookupWorkspace scr x == Just tg
    where
        (Screen (Workspace tg  _ _) scr _) = current x

-- looking at a visible tag
prop_lookup_visible = do
  -- make sure we have some xinerama screens.
  x <- arbitrary `suchThat` \(x' :: T) -> visible x' /= []
  let tags = [ tag (workspace y) | y <- visible x ]
      scr = last [ screen y | y <- visible x ]
  return $ fromJust (lookupWorkspace scr x) `elem` tags


prop_currentTag (x :: T) =
    currentTag x == tag (workspace (current x))

-- Rename a given tag if present in the StackSet.
prop_rename1 (x::T) = do
  o <- arbitraryTag x
  n <- arbitrary `suchThat` \n' -> not $ n' `tagMember` x
  -- Rename o to n
  let y = renameTag o n x
  return $ n `tagMember` y

-- Ensure that a given set of workspace tags is present by renaming
-- existing workspaces and\/or creating new hidden workspaces as
-- necessary.
--
prop_ensure (x :: T) l xs = let y = ensureTags l xs x
                                in and [ n `tagMember` y | n <- xs ]

-- adding a tag should create a new hidden workspace
prop_ensure_append (x :: T) l = do
    n <- arbitrary `suchThat` \n' -> not $ n' `tagMember` x
    let ts = tags x
        y  = ensureTags l (n:ts) x
    return $ hidden y /= hidden x     -- doesn't append, renames
             && and [ isNothing (stack z) && layout z == l | z <- hidden y, tag z == n ]




prop_mapWorkspaceId (x::T) = x == mapWorkspace id x

prop_mapWorkspaceInverse (x::T) = x == mapWorkspace predTag (mapWorkspace succTag x)
  where predTag w = w { tag = pred $ tag w }
        succTag w = w { tag = succ $ tag w }

prop_mapLayoutId (x::T) = x == mapLayout id x

prop_mapLayoutInverse (x::T) = x == mapLayout pred (mapLayout succ x)


