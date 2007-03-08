-----------------------------------------------------------------------------
-- |
-- Module      :  W.hs
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

module WMonad where

import StackSet

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
newtype W a = W { unW :: StateT WState IO a }
    deriving (Functor, Monad, MonadIO, MonadState WState)

-- | Run the W monad, given a chunk of W monad code, and an initial state
-- Return the result, and final state
runW :: WState -> W a -> IO (a, WState)
runW st a = runStateT (unW a) st

-- | Lift an IO action into the W monad
io :: IO a -> W a
io = liftIO

-- | Lift an IO action into the W monad, discarding any result
io_ :: IO a -> W ()
io_ f = liftIO f >> return ()

-- | Run an action forever
forever :: (Monad m) => m a -> m b
forever a = a >> forever a

-- | spawn. Launch an external application
spawn :: String -> W ()
spawn = io_ . runCommand

-- | A 'trace' for the W monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> W ()
trace msg = io $ do
    hPutStrLn stderr msg
    hFlush stderr

-- | Modify the workspace list
modifyWorkspace :: (WorkSpace -> WorkSpace) -> W ()
modifyWorkspace f = do
    modify $ \s -> s { workspace = f (workspace s) }
    ws <- gets workspace
    trace (show $ ws)

-- | Like 'when' but for (WorkSpace -> Maybe a)
whenJust :: (WorkSpace -> Maybe a) -> (a -> W ()) -> W ()
whenJust mg f = do
    ws <- gets workspace
    case mg ws of
        Nothing -> return ()
        Just w  -> f w

