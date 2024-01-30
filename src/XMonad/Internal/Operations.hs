
module XMonad.Internal.Operations
  ( rendered, unsafeLogView
  ) where

import Control.Monad.Reader (asks)
import XMonad.Internal.Core (readView, unsafeWriteView)
import XMonad.Core (X, WindowSet, internal, io, withWindowSet)

-- | Examine the 'WindowSet' that's currently rendered.
rendered :: X WindowSet
rendered = asks internal >>= io . readView

-- | See 'unsafeWriteView'.
unsafeLogView :: X ()
unsafeLogView = do
  i <- asks internal
  withWindowSet (io . unsafeWriteView i)
