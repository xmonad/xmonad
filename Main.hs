-----------------------------------------------------------------------------
-- |
-- Module      :  Main.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-----------------------------------------------------------------------------
--
-- thunk, a minimal window manager for X11
--

import qualified Data.Map as Map
import Data.Map (Map)
import Data.Sequence as Seq
import qualified Data.Foldable as Fold
import Data.Bits
import Control.Monad.State
import System.IO
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import System.Process (runCommand)
import System.Exit

import Wm

handler :: Event -> Wm ()
handler (MapRequestEvent {window = w}) = manage w
handler (DestroyWindowEvent {window = w}) = do
    modifyWindows (Seq.fromList . filter (/= w) . Fold.toList)
    refresh
handler (KeyEvent {event_type = t, state = mod, keycode = code}) 
 | t == keyPress = do
    dpy <- getDisplay
    sym <- io $ keycodeToKeysym dpy code 0
    case filter (\(mod', sym', _) -> mod == mod' && sym == sym') keys of
        []              -> return ()
        ((_, _, act):_) -> act
handler _ = return ()

switch :: Wm ()
switch = do
    ws' <- getWindows
    case viewl ws' of
        EmptyL -> return ()
        (w :< ws) -> do
            setWindows (ws |> w)
            refresh

spawn :: String -> Wm ()
spawn c = do
    io $ runCommand c
    return ()

keys :: [(KeyMask, KeySym, Wm ())]
keys = 
    [ (mod1Mask .|. shiftMask, xK_Return, spawn "xterm")
    , (controlMask, xK_space, spawn "gmrun")
    , (mod1Mask, xK_Tab, switch)
    , (mod1Mask .|. shiftMask, xK_q, io $ exitWith ExitSuccess)
    ]

grabkeys = do
    dpy <- getDisplay
    root <- io $ rootWindow dpy (defaultScreen dpy)
    forM_ keys $ \(mod, sym, _) -> do
        code <- io $ keysymToKeycode dpy sym
        io $ grabKey dpy code mod root True grabModeAsync grabModeAsync

manage :: Window -> Wm ()
manage w = do
    trace "manage"
    d <- getDisplay
    ws <- getWindows
    when (Fold.notElem w ws) $ do
        trace "modifying"
        modifyWindows (w <|)
        io $ mapWindow d w
        refresh

refresh :: Wm ()
refresh = do
    v  <- getWindows
    case viewl v of
        EmptyL   -> return ()
        (w :< _) -> do
            d  <- getDisplay
            sw <- getScreenWidth
            sh <- getScreenHeight
            io $ moveResizeWindow d w 0 0 (fromIntegral sw) (fromIntegral sh)
            io $ raiseWindow d w

main = do
    dpy <- openDisplay ""
    runWm main' (WmState 
                    { display = dpy 
                    , screenWidth  = displayWidth dpy (defaultScreen dpy)
                    , screenHeight = displayHeight dpy (defaultScreen dpy)
                    , windows = Seq.empty
                    })
    return ()

main' = do
    dpy <- getDisplay
    let screen = defaultScreen dpy
    io $ do root <- rootWindow dpy screen
            selectInput dpy root (substructureRedirectMask .|. substructureNotifyMask)
            sync dpy False
    grabkeys
    loop

loop :: Wm ()
loop = do
    dpy <- getDisplay
    e <- io $ allocaXEvent $ \ev -> do
                    nextEvent dpy ev
                    getEvent ev
    handler e
    loop
