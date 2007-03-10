-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-----------------------------------------------------------------------------
--
-- The X monad, a state monad transformer over IO, for the window
-- manager state, and support routines.
--

module XMonad (
    X, WorkSpace, XState(..),runX, withDisplay, io, spawn, trace, whenJust
  ) where

import StackSet (StackSet)

import Control.Monad.State
import System.IO
import System.Process (runCommand)
import Graphics.X11.Xlib (Display,Window)

-- | XState, the window manager state.
-- Just the display, width, height and a window list
data XState = XState
    { display       :: Display
    , screenWidth   :: {-# UNPACK #-} !Int
    , screenHeight  :: {-# UNPACK #-} !Int
    , workspace     :: {-# UNPACK #-} !WorkSpace      -- ^ workspace list
    }

type WorkSpace = StackSet Window

-- | The X monad, a StateT transformer over IO encapuslating the window
-- manager state
newtype X a = X (StateT XState IO a)
    deriving (Functor, Monad, MonadIO, MonadState XState)

-- | Run the X monad, given a chunk of X monad code, and an initial state
-- Return the result, and final state
runX :: XState -> X a -> IO ()
runX st (X a) = runStateT a st >> return ()

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X ()) -> X ()
withDisplay f = gets display >>= f

------------------------------------------------------------------------

-- | Lift an IO action into the X monad
io :: IO a -> X a
io = liftIO
{-# INLINE io #-}

-- | spawn. Launch an external application
spawn :: String -> X ()
spawn x = io (runCommand x) >> return ()

-- | Run a side effecting action with the current workspace. Like 'when' but
whenJust :: Maybe a -> (a -> X ()) -> X ()
whenJust mg f = maybe (return ()) f mg

-- | A 'trace' for the X monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> X ()
trace msg = io $! do hPutStrLn stderr msg; hFlush stderr
