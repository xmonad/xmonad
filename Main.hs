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
import Data.List

import qualified Data.Map as M

import System.IO
import System.Process (runCommand)
import System.Exit

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Control.Monad.State

import W

--
-- The keys list
--
keys :: M.Map (KeyMask, KeySym) (W ())
keys = M.fromList
    [ ((mod1Mask .|. shiftMask, xK_Return), spawn "xterm")
    , ((mod1Mask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((controlMask,            xK_space ), spawn "gmrun")
    , ((mod1Mask,               xK_Tab   ), focus 1)
    , ((mod1Mask,               xK_j     ), focus 1)
    , ((mod1Mask,               xK_k     ), focus (-1))
    , (mod1Mask  .|. shiftMask, xK_c     ), kill)
    , ((mod1Mask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    ]

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

    -- register keys
    registerKeys dpy root = forM_ (M.keys keys) $ \(mod,sym) -> io $ do
        kc <- keysymToKeycode dpy sym
        grabKey dpy kc mod root True grabModeAsync grabModeAsync

--
-- The event handler
-- 
handle :: Event -> W ()
handle (MapRequestEvent {window = w})    = manage w
handle (DestroyWindowEvent {window = w}) = unmanage w
handle (UnmapEvent {window = w})         = unmanage w

handle (KeyEvent {event_type = t, state = mod, keycode = code})
    | t == keyPress = do
        dpy <- gets display
        sym <- io $ keycodeToKeysym dpy code 0
        M.lookup (mod,sym) keys

handle e@(ConfigureRequestEvent {}) = do
    dpy <- gets display
    io $ configureWindow dpy (window e) (value_mask e) $ WindowChanges
            { wcX           = x e
            , wcY           = y e
            , wcWidth       = width e
            , wcHeight      = height e
            , wcBorderWidth = border_width e
            , wcSibling     = above e
            , wcStackMode   = detail e
            }
    io $ sync dpy False

handle _ = return ()

-- ---------------------------------------------------------------------
-- Managing windows

--
-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
--
refresh :: W ()
refresh = do
    ws <- gets windows
    case ws of
        []    -> return ()
        (w:_) -> do
            d  <- gets display
            sw <- liftM fromIntegral (gets screenWidth)
            sh <- liftM fromIntegral (gets screenHeight)
            io $ do moveResizeWindow d w 0 0 sw sh
                    raiseWindow d w

-- | Modify the current window list with a pure funtion, and refresh
withWindows :: (Windows -> Windows) -> W ()
withWindows f = do
    modifyWindows f
    refresh

-- | manage. Add a new window to be managed. Bring it into focus.
manage :: Window -> W ()
manage w = do
    d  <- gets display
    io $ mapWindow d w
    withWindows (nub . (w :))

-- | unmanage, a window no longer exists, remove it from the stack
unmanage :: Window -> W ()
unmanage w = do
    ws <- gets windows
    when (w `elem` ws) $ do
        dpy <- gets display
        io $ do grabServer dpy
                sync dpy False
                ungrabServer dpy
        withWindows $ filter (/= w)

-- | focus. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
focus :: Int -> W ()
focus n = withWindows (rotate n)

-- | spawn. Launch an external application
spawn :: String -> W ()
spawn = io_ . runCommand

-- | Kill the currently focused client
kill :: W ()
kill = do
    ws  <- gets windows
    dpy <- gets display
    case ws of
        []    -> return ()
        (w:_) -> do
        --  if(isprotodel(sel))
        --      sendevent(sel->win, wmatom[WMProtocols], wmatom[WMDelete]);
            io $ killClient dpy w -- ignoring result
            return ()
