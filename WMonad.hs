-----------------------------------------------------------------------------
-- |
-- Module      :  WMonad.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-----------------------------------------------------------------------------
--
-- The W monad, a state monad transformer over IO, for the window
-- manager state, and support routines.
--

module WMonad (
    W, WorkSpace, WState(..),
    runW, withDisplay, io, spawn, trace, whenJust
  ) where

import StackSet (StackSet)

import Control.Monad.State
import System.IO
import System.Process (runCommand)
import Graphics.X11.Xlib (Display,Window)

-- | WState, the window manager state.
-- Just the display, width, height and a window list
data WState = WState
    { display       :: Display
    , screenWidth   :: {-# UNPACK #-} !Int
    , screenHeight  :: {-# UNPACK #-} !Int
    , workspace     :: {-# UNPACK #-} !WorkSpace      -- ^ workspace list
    }

type WorkSpace = StackSet Window

-- | The W monad, a StateT transformer over IO encapuslating the window
-- manager state
newtype W a = W (StateT WState IO a)
    deriving (Functor, Monad, MonadIO, MonadState WState)

-- | Run the W monad, given a chunk of W monad code, and an initial state
-- Return the result, and final state
runW :: WState -> W a -> IO (a, WState)
runW st (W a) = runStateT a st

-- | Run a monad action with the current display settings
withDisplay :: (Display -> W ()) -> W ()
withDisplay f = gets display >>= f

------------------------------------------------------------------------

-- | Lift an IO action into the W monad
io :: IO a -> W a
io = liftIO

-- | spawn. Launch an external application
spawn :: String -> W ()
spawn x = io (runCommand x) >> return ()

-- | Run a side effecting action with the current workspace. Like 'when' but
whenJust :: Maybe a -> (a -> W ()) -> W ()
whenJust mg f = maybe (return ()) f mg

-- | A 'trace' for the W monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> W ()
trace msg = io $ do hPutStrLn stderr msg; hFlush stderr
