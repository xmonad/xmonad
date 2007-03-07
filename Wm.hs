{-# OPTIONS_GHC -fglasgow-exts #-}

module Wm where

import Data.Sequence
import Control.Monad.State
import System.IO (hFlush, hPutStrLn, stderr)
import Graphics.X11.Xlib

data WmState = WmState 
                { display :: Display
                , screenWidth :: Int
                , screenHeight :: Int
                , windows :: Seq Window
                }

newtype Wm a = Wm (StateT WmState IO a)
    deriving (Monad, MonadIO{-, MonadState WmState-})

runWm :: Wm a -> WmState -> IO (a, WmState)
runWm (Wm m) = runStateT m

l :: IO a -> Wm a
l = liftIO

trace msg = l $ do
    hPutStrLn stderr msg
    hFlush stderr

withIO :: (forall b. (a -> IO b) -> IO b) -> (a -> Wm c) -> Wm c
withIO f g = do
    s <- Wm get
    (y, s') <- l $ f $ \x -> runWm (g x) s
    Wm (put s')
    return y

getDisplay = Wm (gets display)

getWindows = Wm (gets windows)

getScreenWidth = Wm (gets screenWidth)

getScreenHeight = Wm (gets screenHeight)

setWindows x = Wm (modify (\s -> s {windows = x}))

modifyWindows :: (Seq Window -> Seq Window) -> Wm ()
modifyWindows f = Wm (modify (\s -> s {windows = f (windows s)}))
