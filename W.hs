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

module W where

import Data.Sequence
import Control.Monad.State
import System.IO (hFlush, hPutStrLn, stderr)
import Graphics.X11.Xlib

--
-- | WState, the window manager state.
-- Just the display, width, height and a window list
--
data WState = WState
    { display       :: Display
    , screenWidth   :: !Int
    , screenHeight  :: !Int
    , windows       :: Seq Window
    }

-- | The W monad, a StateT transformer over IO encapuslating the window
-- manager state
--
newtype W a = W (StateT WState IO a)
    deriving (Functor, Monad, MonadIO)

-- | Run the W monad, given a chunk of W monad code, and an initial state
-- Return the result, and final state
--
runW :: W a -> WState -> IO (a, WState)
runW (W m) = runStateT m

withIO :: (forall b. (a -> IO b) -> IO b) -> (a -> W c) -> W c
withIO f g = do
    s <- W get
    (y, t) <- io (f (flip runW s . g))
    W (put t)
    return y

--
-- | Lift an IO action into the W monad
--
io :: IO a -> W a
io = liftIO

--
-- | Lift an IO action into the W monad, discarding any result
--
io_ :: IO a -> W ()
io_ f = liftIO f >> return ()

--
-- | A 'trace' for the W monad
--
trace :: String -> W ()
trace msg = io $ do
    hPutStrLn stderr msg
    hFlush stderr

--
-- | Run an action forever
--
forever :: (Monad m) => m a -> m b
forever a = a >> forever a

-- ---------------------------------------------------------------------
-- Getting at the window manager state

-- | Return the current dispaly
getDisplay          :: W Display
getDisplay          = W (gets display)

-- | Return the current windows
getWindows          :: W (Seq Window)
getWindows          = W (gets windows)

-- | Return the screen width
getScreenWidth      :: W Int
getScreenWidth      = W (gets screenWidth)

-- | Return the screen height
getScreenHeight     :: W Int
getScreenHeight     = W (gets screenHeight)

-- | Set the current window list
setWindows          :: Seq Window -> W ()
setWindows x        = W (modify (\s -> s {windows = x}))

-- | Modify the current window list
modifyWindows       :: (Seq Window -> Seq Window) -> W ()
modifyWindows f     = W (modify (\s -> s {windows = f (windows s)}))
