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

import Data.List
import Data.Bits hiding (rotate)
import qualified Data.Map as M

import System.IO
import System.Exit

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Control.Monad.State

import WMonad
import qualified StackSet as W

--
-- The number of workspaces:
--
workspaces :: Int
workspaces = 5

--
-- The keys list
--
keys :: M.Map (KeyMask, KeySym) (W ())
keys = M.fromList $
    [ ((mod1Mask .|. shiftMask, xK_Return), spawn "xterm")
    , ((mod1Mask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((controlMask,            xK_space ), spawn "gmrun")
    , ((mod1Mask,               xK_Tab   ), focus GT)
    , ((mod1Mask,               xK_j     ), focus GT)
    , ((mod1Mask,               xK_k     ), focus LT)
    , ((mod1Mask .|. shiftMask, xK_c     ), kill)
    , ((mod1Mask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    ] ++
    -- generate keybindings to each workspace:
    [((m .|. mod1Mask, xK_0 + fromIntegral i), f i)
        | i <- [1 .. workspaces]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]


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
            , workspace    = W.empty workspaces
            }

    runW initState $ do
        r <- io $ rootWindow dpy dflt
        io $ do selectInput dpy r (substructureRedirectMask .|. substructureNotifyMask)
                sync dpy False
        registerKeys dpy r
        (_, _, ws) <- io $ queryTree dpy r
        forM_ ws $ \w -> do
            wa <- io $ getWindowAttributes dpy w
            when (waMapState wa == waIsViewable) (manage w)
        go dpy

    return ()
  where
    -- The main loop
    go dpy = forever $ do
        e <- io $ allocaXEvent $ \ev -> nextEvent dpy ev >> getEvent ev
        handle e

    -- register keys
    registerKeys dpy r = forM_ (M.keys keys) $ \(m,s) -> io $ do
        kc <- keysymToKeycode dpy s
        grabKey dpy kc m r True grabModeAsync grabModeAsync

--
-- | handle. Handle X events
-- 
handle :: Event -> W ()
handle (MapRequestEvent    {window = w}) = manage w
handle (DestroyWindowEvent {window = w}) = unmanage w
handle (UnmapEvent         {window = w}) = unmanage w

handle (KeyEvent {event_type = t, state = m, keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        s   <- io $ keycodeToKeysym dpy code 0
        maybe (return ()) id (M.lookup (m,s) keys)

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

handle e = trace (eventName e) -- return ()

-- ---------------------------------------------------------------------
-- Managing windows

-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
refresh :: W ()
refresh = whenJust W.peek $ \w -> withScreen $ \(d,sw,sh) -> io $ do
    moveResizeWindow d w 0 0 (fromIntegral sw) (fromIntegral sh) -- fullscreen
    raiseWindow d w

-- | hide. Hide a list of windows by moving them offscreen.
hide :: Window -> W ()
hide w = withScreen $ \(dpy,sw,sh) -> io $
    moveWindow dpy w (2*fromIntegral sw) (2*fromIntegral sh)

-- | reveal. Expose a list of windows, moving them on screen
reveal :: Window -> W ()
reveal w = withDisplay $ \d -> io $ moveWindow d w 0 0

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WorkSpace -> WorkSpace) -> W ()
windows f = modifyWorkspace f >> refresh

-- ---------------------------------------------------------------------
-- Window operations

-- | manage. Add a new window to be managed in the current workspace. Bring it into focus.
-- If the window is already under management, it is just raised.
manage :: Window -> W ()
manage w = do withDisplay $ \d -> io $ mapWindow d w
              windows $ W.push w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
unmanage :: Window -> W ()
unmanage w = do
    ws <- gets workspace
    when (W.member w ws) $ do
        withDisplay $ \d -> io $ withServer d $ sync d False
        windows $ W.delete w

-- | focus. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
focus :: Ordering -> W ()
focus = windows . W.rotate

-- | Kill the currently focused client
kill :: W ()
kill = withDisplay $ \d -> whenJust W.peek $ io_ . killClient d

-- | tag. Move a window to a new workspace
tag :: Int -> W ()
tag o = do
    ws <- gets workspace
    when (n /= W.cursor ws) $
        whenJust W.peek $ \w -> do
            hide w
            windows $ W.shift n
    where n = o -1

-- | view. Change the current workspace to workspce at offset 'n-1'.
view :: Int -> W ()
view o = do
    ws <- gets workspace
    when (n /= W.cursor ws) $
        whenJust (flip W.index n) $ \new -> do
            mapM_ hide (W.stack ws)
            mapM_ reveal new
            windows $ W.view n
    where n = o-1

