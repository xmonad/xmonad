-----------------------------------------------------------------------------
-- |
-- Module      :  Main.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  sjanssen@cse.unl.edu
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-----------------------------------------------------------------------------
--
-- thunk, a minimal window manager for X11
--

import Data.Bits hiding (rotate)

import System.IO
import System.Process (runCommand)
import System.Exit

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Control.Monad.State

import W

--
-- let's get underway
-- 
main :: IO ()
main = do
    dpy <- openDisplay ""
    let dflt      = defaultScreen dpy
        initState = WState
            { display      = dpy
            , screenWidth  = displayWidth  dpy dflt
            , screenHeight = displayHeight dpy dflt
            , windows      = [] }

    runW initState $ do
        root <- io $ rootWindow dpy dflt
        io $ do selectInput dpy root (substructureRedirectMask .|. substructureNotifyMask)
                sync dpy False
        registerKeys dpy root
        go dpy

    return ()
  where
    -- The main loop
    go dpy = forever $ do
        e <- io $ allocaXEvent $ \ev -> nextEvent dpy ev >> getEvent ev
        handle e

--
-- | grabkeys. Register key commands
--
registerKeys :: Display -> Window -> W ()
registerKeys dpy root =
    forM_ keys $ \(mod, sym, _) -> do
        kc <- io (keysymToKeycode dpy sym)
        io $ grabKey dpy kc mod root True grabModeAsync grabModeAsync

keys :: [(KeyMask, KeySym, W ())]
keys =
    [ (mod1Mask .|. shiftMask, xK_Return, spawn "xterm")
    , (mod1Mask,               xK_p,      spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , (controlMask,            xK_space,  spawn "gmrun")
    , (mod1Mask,               xK_Tab,    switch)
    , (mod1Mask .|. shiftMask, xK_q,      io $ exitWith ExitSuccess)
    ]

--
-- The event handler
-- 
handle :: Event -> W ()
handle (MapRequestEvent {window = w}) = manage w

handle (DestroyWindowEvent {window = w}) = do
    modifyWindows (filter (/= w))
    refresh

handle (KeyEvent {event_type = t, state = mod, keycode = code})
    | t == keyPress = do
        dpy <- getDisplay
        sym <- io $ keycodeToKeysym dpy code 0
        case filter (\(mod', sym', _) -> mod == mod' && sym == sym') keys of
            []              -> return ()
            ((_, _, act):_) -> act

handle _ = return ()

-- ---------------------------------------------------------------------
-- Managing windows

-- | Modify the current window list with a pure funtion, and refresh
withWindows :: (Windows -> Windows) -> W ()
withWindows f = do
    modifyWindows f
    refresh

-- |  Run an action on the currently focused window
withCurrent :: (Window -> W ()) -> W ()
withCurrent f = do
    ws <- getWindows
    case ws of
        []    -> return ()
        (w:_) -> f w

--
-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
--
refresh :: W ()
refresh = withCurrent $ \w -> do
    d  <- getDisplay
    sw <- getScreenWidth
    sh <- getScreenHeight
    io $ do moveResizeWindow d w 0 0 (fromIntegral sw) (fromIntegral sh)
            raiseWindow d w

--
-- | manage. Add a new window to be managed
--
manage :: Window -> W ()
manage w = do
    trace "manage"
    d  <- getDisplay
    withWindows $ \ws -> if w `elem` ws then ws else w:ws -- a set
    io $ mapWindow d w


--
-- | switch. switch focus to next window in list.
-- The currently focused window is always the head of the list
--
switch :: W ()
switch = withWindows rotate

--
-- | spawn. Launch an external application
--
spawn :: String -> W ()
spawn = io_ . runCommand
