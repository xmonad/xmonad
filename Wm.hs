-----------------------------------------------------------------------------
-- |
-- Module      :  Wm.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-----------------------------------------------------------------------------
--
-- The Wm monad, a state monad transformer over IO, for the window manager state.
--

module Wm where

import Data.Sequence
import Control.Monad.State
import System.IO (hFlush, hPutStrLn, stderr)
import Graphics.X11.Xlib

data WmState = WmState
    { display       :: Display
    , screenWidth   :: !Int
    , screenHeight  :: !Int
    , windows       :: Seq Window
    }

newtype Wm a = Wm (StateT WmState IO a)
    deriving (Monad, MonadIO{-, MonadState WmState-})

runWm :: Wm a -> WmState -> IO (a, WmState)
runWm (Wm m) = runStateT m

io :: IO a -> Wm a
io = liftIO

trace msg = io $ do
    hPutStrLn stderr msg
    hFlush stderr

withIO :: (forall b. (a -> IO b) -> IO b) -> (a -> Wm c) -> Wm c
withIO f g = do
    s <- Wm get
    (y, s') <- io $ f $ \x -> runWm (g x) s
    Wm (put s')
    return y

getDisplay = Wm (gets display)

getWindows = Wm (gets windows)

getScreenWidth = Wm (gets screenWidth)

getScreenHeight = Wm (gets screenHeight)

setWindows x = Wm (modify (\s -> s {windows = x}))

modifyWindows :: (Seq Window -> Seq Window) -> Wm ()
modifyWindows f = Wm (modify (\s -> s {windows = f (windows s)}))
