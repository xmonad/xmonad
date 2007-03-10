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
-- xmonad, a minimal window manager for X11
--

import Data.List
import Data.Bits hiding (rotate)
import qualified Data.Map as M

import System.IO
import System.Exit

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import Numeric
import Control.Monad.State

import XMonad
import qualified StackSet as W

--
-- The number of workspaces:
--
workspaces :: Int
workspaces = 9

--
-- The keys list
--
keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
    [ ((mod1Mask .|. shiftMask, xK_Return), spawn "xterm")
    , ((mod1Mask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((controlMask,            xK_space ), spawn "gmrun")
    , ((mod1Mask,               xK_Tab   ), raise GT)
    , ((mod1Mask,               xK_j     ), raise GT)
    , ((mod1Mask,               xK_k     ), raise LT)
    , ((mod1Mask .|. shiftMask, xK_c     ), kill)
    , ((mod1Mask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    ] ++
    -- generate keybindings to each workspace:
    [((m .|. mod1Mask, xK_0 + fromIntegral i), f i)
        | i <- [1 .. workspaces]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]

--
-- The main entry point
-- 
main :: IO ()
main = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
        st   = XState
            { display      = dpy
            , screenWidth  = displayWidth  dpy dflt
            , screenHeight = displayHeight dpy dflt
            , workspace    = W.empty workspaces
            }

    -- setup initial X environment
    rootw <- rootWindow dpy dflt
    sync dpy False
    selectInput dpy rootw $  substructureRedirectMask
                         .|. substructureNotifyMask
                         .|. enterWindowMask
                         .|. leaveWindowMask
    grabKeys dpy rootw
    sync dpy False

    ws   <- scan dpy rootw
    allocaXEvent $ \e ->
        runX st $ do
            mapM_ manage ws
            forever $ handle =<< xevent dpy e
      where
        xevent d e = io (nextEvent d e >> getEvent e)

        forever a = a >> forever a

-- ---------------------------------------------------------------------
-- IO stuff. Doesn't require any X state
-- Most of these things run only on startup (bar grabkeys)

-- | scan for any initial windows to manage
scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_, _, ws) <- queryTree dpy rootw
    filterM ok ws
  where
    ok w = do wa <- getWindowAttributes dpy w
              return $ not (waOverrideRedirect wa)
                     && waMapState wa == waIsViewable

-- | Grab the keys back
grabKeys :: Display -> Window -> IO ()
grabKeys dpy rootw = do
    ungrabKey dpy '\0' {-AnyKey-} anyModifier rootw
    forM_ (M.keys keys) $ \(mask,sym) -> do
         kc <- keysymToKeycode dpy sym
         mapM_ (grab kc) [mask, mask .|. lockMask] -- note: no numlock
  where
    grab kc m = grabKey dpy kc m rootw True grabModeAsync grabModeAsync

-- ---------------------------------------------------------------------
-- Event handler
--
-- | handle. Handle X events
--
-- Events dwm handles that we don't:
--
--    [ButtonPress]    = buttonpress,
--    [Expose]         = expose,
--    [PropertyNotify] = propertynotify,
--
-- Todo: seperate IO from X monad stuff. We want to be able to test the
-- handler, and client functions, with dummy X interface ops, in QuickCheck
--
-- Will require an abstract interpreter from Event -> X Action, which
-- modifies the internal X state, and then produces an IO action to
-- evaluate.
--
-- XCreateWindowEvent(3X11)
-- Window manager clients normally should ignore this window if the
-- override_redirect member is True.
-- 
handle :: Event -> X ()

handle (MapRequestEvent    {window = w}) = withDisplay $ \dpy -> do
    wa <- io $ getWindowAttributes dpy w
    when (not (waOverrideRedirect wa)) $ manage w

handle (DestroyWindowEvent {window = w}) = unmanage w
handle (UnmapEvent         {window = w}) = unmanage w

handle e@(MappingNotifyEvent {window = w}) = do
    let m = (request e, first_keycode e, count e)
    io $ refreshKeyboardMapping m
    when (request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

handle (KeyEvent {event_type = t, state = m, keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        s   <- io $ keycodeToKeysym dpy code 0
        maybe (return ()) id (M.lookup (m,s) keys)

handle e@(CrossingEvent {event_type = t})
    | t == enterNotify  && mode e == notifyNormal && detail e /= notifyInferior
    = withDisplay $ \d -> do
        let w = window e
        ws <- gets workspace
        -- note: we get enter events for what appear to be subwindows of
        -- ones under managment. we need to ignore those. hence we check either for
        -- root, or for ismember.
        if W.member w ws
            then io $ setInputFocus d w revertToPointerRoot 0 -- it is ours, manage it
            else do let dflt = defaultScreen d
                    rootw <- io $ rootWindow d dflt -- should be in state
                    when (w == rootw) $ do
                       let w' = maybe rootw id (W.peek ws) -- focus to the top of the stack
                       io $ setInputFocus d w' revertToPointerRoot 0

handle e@(CrossingEvent {event_type = t})
    | t == leaveNotify
    = withDisplay $ \d -> do
        let dflt = defaultScreen d
        rootw <- io $ rootWindow d dflt
        when (window e == rootw && not (same_screen e)) $
          io $ setInputFocus d rootw revertToPointerRoot 0

handle e@(ConfigureRequestEvent {}) = do
    dpy <- gets display
    ws  <- gets workspace
    let w = window e

    when (W.member w ws) $ -- already managed, reconfigure (see client:configure() 
        trace ("Reconfigure already managed window: " ++ show w)

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

handle e = trace (eventName e) -- ignoring

-- ---------------------------------------------------------------------
-- Managing windows

-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
refresh :: X ()
refresh = do
    ws <- gets workspace
    whenJust (W.peek ws) $ \w ->
        withDisplay $ \d -> do
            sw <- gets screenWidth
            sh <- gets screenHeight
            io $ do moveResizeWindow d w 0 0 (fromIntegral sw) (fromIntegral sh) -- fullscreen
                    raiseWindow d w

-- | hide. Hide a list of windows by moving them offscreen.
hide :: Window -> X ()
hide w = withDisplay $ \d -> do
    sw <- gets screenWidth
    sh <- gets screenHeight
    io $! moveWindow d w (2*fromIntegral sw) (2*fromIntegral sh)

-- | reveal. Expose a list of windows, moving them on screen
reveal :: Window -> X ()
reveal w = withDisplay $ \d -> io $! moveWindow d w 0 0

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WorkSpace -> WorkSpace) -> X ()
windows f = do
    modify $ \s -> s { workspace = f (workspace s) }
    refresh
    ws <- gets workspace
    trace (show ws) -- log state changes to stderr

-- ---------------------------------------------------------------------
-- Window operations

-- | manage. Add a new window to be managed in the current workspace. Bring it into focus.
-- If the window is already under management, it is just raised.
--
-- When we start to manage a window, it gains focus.
--
manage :: Window -> X ()
manage w = do
    trace ("Managing window: 0x" ++ showHex w (", " ++ show w))
    withDisplay $ \d -> io $ do
        selectInput d w $ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
        mapWindow d w
        setInputFocus d w revertToPointerRoot 0 -- CurrentTime
    windows $ W.push w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
unmanage :: Window -> X ()
unmanage w = do
    ws <- gets workspace
    when (W.member w ws) $ do
        withDisplay $ \d -> io $ withServer d $ sync d False
        windows $ W.delete w

-- | raise. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
raise :: Ordering -> X ()
raise = windows . W.rotate

-- | Kill the currently focused client
kill :: X ()
kill = withDisplay $ \d -> do
    ws <- gets workspace
    whenJust (W.peek ws) $ \w -> do
        trace ("Attempting to kill window: 0x" ++ showHex w (", " ++ show w))
        io (killClient d w) >> return ()

-- | tag. Move a window to a new workspace
tag :: Int -> X ()
tag o = do
    ws <- gets workspace
    let m = W.current ws
    when (n /= m) $
        whenJust (W.peek ws) $ \w -> do
            hide w
            windows $ W.shift n
    where n = o-1

-- | view. Change the current workspace to workspce at offset 'n-1'.
view :: Int -> X ()
view o = do
    ws <- gets workspace
    let m = W.current ws
    when (n /= m) $ do
        mapM_ reveal (W.index n ws)
        mapM_ hide   (W.index m ws)
        windows $ W.view n
    where n = o-1

