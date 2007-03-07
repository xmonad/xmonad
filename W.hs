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

import Control.Monad.State
import System.IO
import Graphics.X11.Xlib (Display,Window)
import qualified Data.Sequence as S

-- | WState, the window manager state.
-- Just the display, width, height and a window list
data WState = WState
    { display       :: Display
    , screenWidth   :: !Int
    , screenHeight  :: !Int
    , workspace     :: !WorkSpaces      -- ^ workspace list
    }

type WorkSpaces = (Int, S.Seq Windows)
type Windows    = [Window]

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

-- | A 'trace' for the W monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: String -> W ()
trace msg = io $ do
    hPutStrLn stderr msg
    hFlush stderr

-- ---------------------------------------------------------------------
-- Getting at the window manager state

-- | Modify the workspace list
modifyWorkspaces   :: (WorkSpaces -> WorkSpaces) -> W ()
modifyWorkspaces f = modify $ \s -> s { workspace = f (workspace s) }

-- | Modify the current window list
modifyWindows      :: (Windows -> Windows) -> W ()
modifyWindows    f = modifyWorkspaces $ \(i,wk) -> (i, S.adjust f i wk)

-- ---------------------------------------------------------------------
-- Generic utilities

-- | Run an action forever
forever :: (Monad m) => m a -> m b
forever a = a >> forever a

-- | Rotate a list by 'n' elements.
--
--  rotate 0    -->  [5,6,7,8,1,2,3,4]
--  rotate 1    -->  [6,7,8,1,2,3,4,5]
--  rotate (-1) -->  [4,5,6,7,8,1,2,3]
--
--  where xs = [5..8] ++ [1..4]
--
rotate :: Int -> [a] -> [a]
rotate n xs = take l . drop offset . cycle $ xs
  where
    l      = length xs
    offset | n < 0     = l + n
           | otherwise = n

