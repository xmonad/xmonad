{-# LANGUAGE NamedFieldPuns #-}

module XMonad.Internal.Core
  ( Internal, unsafeMakeInternal
  , readView, unsafeWriteView
  ) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

-- | An opaque data type for holding state and configuration that isn't to be
--   laid bare to the world outside, nor even to the rest of the package if we
--   can help it.
newtype Internal model = Internal
  { view :: IORef model -- ^ An 'IORef' to which we log the state of the view.
  }

-- | The ability to construct an 'Internal' allows one to play tricks with
--   'local'.
unsafeMakeInternal :: model -> IO (Internal model)
unsafeMakeInternal model = do
  viewRef <- newIORef model
  pure Internal
    { view = viewRef
    }

readView :: Internal model -> IO model
readView Internal{view} = readIORef view

-- | The 'view' ref can only be safely written to with a just-rendered model.
unsafeWriteView :: Internal model -> model -> IO ()
unsafeWriteView Internal{view} = writeIORef view

