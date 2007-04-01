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
    X, WorkSpace, XState(..), Layout(..), LayoutDesc(..), Disposition(..),
    basicLayoutDesc, currentDesc, disposition,
    runX, io, withDisplay, isRoot,
    spawn, trace, whenJust, swap
  ) where

import StackSet (StackSet)
import qualified StackSet as W
import Data.Ratio

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
    , layoutDescs   :: {-# UNPACK #-} !(M.Map Int LayoutDesc)
    , dispositions  :: {-# UNPACK #-} !(M.Map Window Disposition)
    -- ^ mapping of workspaces to descriptions of their layouts
    }

type WorkSpace = StackSet Window


-- ---------------------------------------------------------------------
-- Dispositions and Layout

-- | Disposition.  Short for 'Display Position,' it describes how much
-- of the screen a window would like to occupy, when tiled with others.
data Disposition
    = Disposition { vertFrac, horzFrac :: {-# UNPACK #-} !Rational }

basicDisposition :: Disposition
basicDisposition = Disposition (1%3) (1%3)

-- | The different layout modes
data Layout = Full | Horz | Vert

-- | 'not' for Layout.
swap :: Layout -> Layout
swap Full = Tile
swap _    = Full

-- | A full description of a particular workspace's layout parameters.
data LayoutDesc = LayoutDesc { layoutType   :: !Layout,
                               horzTileFrac :: !Rational,
                               vertTileFrac :: !Rational }

basicLayoutDesc :: LayoutDesc
basicLayoutDesc = LayoutDesc { layoutType = Full,
                               horzTileFrac = 1%2,
                               vertTileFrac = 1%2 }

-- | disposition. Gets the disposition of a particular window.
disposition :: Window -> XState -> Disposition
disposition w s = M.findWithDefault basicDisposition w (dispositions s)

-- | Gets the current layoutDesc.
currentDesc :: XState -> LayoutDesc
currentDesc s =  M.findWithDefault basicLayoutDesc n (layoutDescs s)
    where n = (W.current . workspace $ s)



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
