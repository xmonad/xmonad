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
    X, WorkSpace, XState(..), Layout(..), LayoutDesc(..),
    runX, io, withDisplay, isRoot,
    spawn, trace, whenJust, rotateLayout
  ) where

import StackSet (StackSet,WorkspaceId)

import Control.Monad.State
import System.IO
import System.Posix.Process (executeFile, forkProcess, getProcessStatus)
import System.Exit
import Graphics.X11.Xlib

import qualified Data.Map as M

-- | XState, the window manager state.
-- Just the display, width, height and a window list
data XState = XState
    { display           :: Display                         -- ^ the X11 display

    , theRoot           :: !Window                         -- ^ the root window
    , wmdelete          :: !Atom                           -- ^ window deletion atom
    , wmprotocols       :: !Atom                           -- ^ wm protocols atom
    , dimensions        :: !(Int,Int)                      -- ^ dimensions of the screen, 
                                                           -- used for hiding windows
    , workspace         :: !WorkSpace                      -- ^ workspace list

    , xineScreens       :: ![Rectangle]                    -- ^ dimensions of each screen
    , defaultLayoutDesc :: !LayoutDesc                     -- ^ default layout
    , layoutDescs       :: !(M.Map WorkspaceId LayoutDesc) -- ^ mapping of workspaces 
                                                           -- to descriptions of their layouts
    }

type WorkSpace = StackSet Window

------------------------------------------------------------------------

-- | The X monad, a StateT transformer over IO encapsulating the window
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

------------------------------------------------------------------------
-- Layout handling

-- | The different layout modes
data Layout = Full | Tall | Wide deriving (Enum, Bounded, Eq)

-- | 'rot' for Layout.
rotateLayout :: Layout -> Layout
rotateLayout x = if x == maxBound then minBound else succ x

-- | A full description of a particular workspace's layout parameters.
data LayoutDesc = LayoutDesc { layoutType   :: !Layout
                             , tileFraction :: !Rational
                             }

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
