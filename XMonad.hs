{-# OPTIONS -fglasgow-exts #-}
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
    X, WindowSet, WorkspaceId(..), ScreenId(..), XState(..), XConf(..), Layout(..),
    Typeable, Message, SomeMessage(..), fromMessage, atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW,
    runX, io, withDisplay, withWindowSet, isRoot, spawn, restart, trace, whenJust, whenX
  ) where

import StackSet (StackSet)

import Control.Monad.State
import Control.Monad.Reader
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import System.Posix.Process (executeFile, forkProcess, getProcessStatus, createSession)
import System.Exit
import System.Environment
import Graphics.X11.Xlib
import Data.Typeable

import qualified Data.Map as M

-- | XState, the window manager state.
-- Just the display, width, height and a window list
data XState = XState
    { windowset   :: !WindowSet           -- ^ workspace list
    , xineScreens :: ![Rectangle]         -- ^ dimensions of each screen
    , dimensions  :: !(Position,Position) -- ^ dimensions of the screen,
    , statusGaps  :: ![(Int,Int,Int,Int)] -- ^ width of status bar on each screen
    , layouts     :: !(M.Map WorkspaceId (Layout, [Layout]))  }
                       -- ^ mapping of workspaces to descriptions of their layouts

data XConf = XConf
    { display       :: Display      -- ^ the X11 display
    , theRoot       :: !Window      -- ^ the root window
    , normalBorder  :: !Color       -- ^ border color of unfocused windows
    , focusedBorder :: !Color     } -- ^ border color of the focused window

type WindowSet = StackSet WorkspaceId Window ScreenId

-- | Virtual workspace indicies
newtype WorkspaceId = W Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | Physical screen indicies
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

------------------------------------------------------------------------

-- | The X monad, a StateT transformer over IO encapsulating the window
-- manager state
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on XConf and XState automatically.
--
newtype X a = X (ReaderT XConf (StateT XState IO) a)
    deriving (Functor, Monad, MonadIO, MonadState XState, MonadReader XConf)

-- | Run the X monad, given a chunk of X monad code, and an initial state
-- Return the result, and final state
runX :: XConf -> XState -> X a -> IO ()
runX c st (X a) = runStateT (runReaderT a c) st >> return ()

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X a) -> X a
withDisplay f = asks display >>= f

-- | Run a monadic action with the current stack set
withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

-- | True if the given window is the root window
isRoot :: Window -> X Bool
isRoot w = liftM (w==) (asks theRoot)

-- | Wrapper for the common case of atom internment
getAtom :: String -> X Atom
getAtom str = withDisplay $ \dpy -> io $ internAtom dpy str False

-- | Common non-predefined atoms
atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW :: X Atom
atom_WM_PROTOCOLS = getAtom "WM_PROTOCOLS"
atom_WM_DELETE_WINDOW = getAtom "WM_DELETE_WINDOW"

------------------------------------------------------------------------
-- Layout handling

-- | The different layout modes
-- 'doLayout', a pure function to layout a Window set 'modifyLayout', 
-- 'modifyLayout' can be considered a branch of an exception handler.
--
data Layout = Layout { doLayout     :: Rectangle -> [Window] -> X [(Window, Rectangle)]
                     , modifyLayout :: SomeMessage -> Maybe Layout }

-- Based on ideas in /An Extensible Dynamically-Typed Hierarchy of Exceptions/,
-- Simon Marlow, 2006. Use extensible messages to the modifyLayout handler.
-- 
-- User-extensible messages must be a member of this class:
--
class Typeable a => Message a

--
-- A wrapped value of some type in the Message class.
--
data SomeMessage = forall a. Message a => SomeMessage a

--
-- And now, unwrap a given, unknown Message type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- ---------------------------------------------------------------------
-- General utilities

-- | Lift an IO action into the X monad
io :: IO a -> X a
io = liftIO

-- | spawn. Launch an external application
spawn :: String -> X ()
spawn x = io $ do
    pid <- forkProcess $ do
        forkProcess (createSession >> executeFile "/bin/sh" False ["-c", x] Nothing)
        exitWith ExitSuccess
    getProcessStatus True False pid
    return ()

-- | Restart xmonad via exec().
--
-- If the first parameter is 'Just name', restart will attempt to execute the
-- program corresponding to 'name'.  Otherwise, xmonad will attempt to execute
-- the name of the current program.
--
-- When the second parameter is 'True', xmonad will attempt to resume with the
-- current window state.
restart :: Maybe String -> Bool -> X ()
restart mprog resume = do
    prog <- maybe (io $ getProgName) return mprog
    args <- if resume then gets (("--resume":) . return . show . windowset) else return []
    io $ catch (executeFile prog True args Nothing)
               (hPutStrLn stderr . show) -- print executable not found exception

-- | Run a side effecting action with the current workspace. Like 'when' but
whenJust :: Maybe a -> (a -> X ()) -> X ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, using a X event to decide
whenX :: X Bool -> X () -> X ()
whenX a f = a >>= \b -> when b f

-- | Grab the X server (lock it) from the X monad
-- withServerX :: X () -> X ()
-- withServerX f = withDisplay $ \dpy -> do
--     io $ grabServer dpy
--     f
--     io $ ungrabServer dpy

-- | A 'trace' for the X monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> X ()
trace msg = io $! do hPutStrLn stderr msg; hFlush stderr
