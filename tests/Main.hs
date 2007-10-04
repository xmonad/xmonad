module Main where

import qualified Properties

-- This will run all of the QC files for xmonad core. Currently, that's just
-- Properties. If any more get added, sequence the main actions together.
main = do
  Properties.main
