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
    X, WorkSpace, XState(..), Layout(..),
    runX, io, withDisplay, isRoot,
    spawn, trace, whenJust
  ) where

import StackSet (StackSet)

import Control.Monad.State
import System.IO
import System.Posix.Process (executeFile, forkProcess, getProcessStatus)
import System.Exit
import Graphics.X11.Xlib

import qualified Data.Map as M

-- | XState, the window manager state.
-- Just the display, width, height and a window list
data XState = XState
    { display       :: Display
    , screen        :: {-# UNPACK #-} !ScreenNumber
    , xineScreens   :: {-# UNPACK #-} ![Rectangle]
    -- a mapping of workspaces to xinerama screen numbers
    , wsOnScreen    :: {-# UNPACK #-} !(M.Map Int Int)
    , theRoot       :: {-# UNPACK #-} !Window
    , wmdelete      :: {-# UNPACK #-} !Atom
    , wmprotocols   :: {-# UNPACK #-} !Atom
    , dimensions    :: {-# UNPACK #-} !(Int,Int)
    , workspace     :: {-# UNPACK #-} !WorkSpace      -- ^ workspace list
    , layout        :: {-# UNPACK #-} !Layout
    }

type WorkSpace = StackSet Window

-- | The different layout modes
data Layout = Full | Tile

-- | The X monad, a StateT transformer over IO encapuslating the window
-- manager state
newtype X a = X (StateT XState IO a)
    deriving (Functor, Monad, MonadIO, MonadState XState)

-- | Run the X monad, given a chunk of X monad code, and an initial state
-- Return the result, and final state
runX :: XState -> X a -> IO ()
runX st (X a) = runStateT a st >> return ()

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X ()) -> X ()
withDisplay f = gets display >>= f

-- | True if the given window is the root window
isRoot :: Window -> X Bool
isRoot w = liftM (w==) (gets theRoot)

-- ---------------------------------------------------------------------
-- Utilities

-- | Lift an IO action into the X monad
io :: IO a -> X a
io = liftIO
{-# INLINE io #-}

-- | spawn. Launch an external application
spawn :: String -> X ()
spawn x = io $ do
    pid <- forkProcess $ do
        forkProcess (executeFile "/bin/sh" False ["-c", x] Nothing)
        exitWith ExitSuccess
        return ()
    getProcessStatus True False pid
    return ()

-- | Run a side effecting action with the current workspace. Like 'when' but
whenJust :: Maybe a -> (a -> X ()) -> X ()
whenJust mg f = maybe (return ()) f mg

-- | A 'trace' for the X monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> X ()
trace msg = io $! do hPutStrLn stderr msg; hFlush stderr
