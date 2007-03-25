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
import Data.Maybe
import Data.Ratio
import Data.Bits hiding (rotate)
import qualified Data.Map as M

import System.IO
import System.Exit

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Control.Monad.State

import System.Posix.Process
import System.Environment

import XMonad
import qualified StackSet as W

--
-- The number of workspaces:
--
workspaces :: Int
workspaces = 9

--
-- modMask lets you easily change which modkey you use.
--
modMask :: KeyMask
modMask = mod1Mask

--
-- The keys list
--
keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn "xterm")
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((controlMask,           xK_space ), spawn "gmrun")
    , ((modMask,               xK_Tab   ), raise GT)
    , ((modMask,               xK_j     ), raise GT)
    , ((modMask,               xK_k     ), raise LT)
    , ((modMask,               xK_h     ), changeWidth (negate defaultDelta))
    , ((modMask,               xK_l     ), changeWidth defaultDelta)
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)
    , ((modMask .|. shiftMask, xK_F12   ), io restart)
    , ((modMask,               xK_space ), switchLayout)
    , ((modMask,               xK_Return), promote)
    ] ++
    -- generate keybindings to each workspace:
    [((m .|. modMask, xK_0 + fromIntegral i), f i)
        | i <- [1 .. workspaces]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]
    -- generate keybindings to each screen:
    ++
    [((m .|. modMask, key), screenWS sc >>= f)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [1..]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]


-- The default size for the left pane
defaultLeftWidth :: Rational
defaultLeftWidth = 1%2

-- How much to change the size of the windows on the left by default
defaultDelta :: Rational
defaultDelta = 3%100

--
-- The mask for the numlock key.  You may need to change this on some systems.
--
numlockMask :: KeySym
numlockMask = lockMask

--
-- The main entry point
-- 
main :: IO ()
main = do
    dpy   <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw  <- rootWindow dpy dflt
    wmdelt <- internAtom dpy "WM_DELETE_WINDOW" False
    wmprot <- internAtom dpy "WM_PROTOCOLS"     False
    xinesc <- getScreenInfo dpy

    let st = XState
            { display      = dpy
            , screen       = dflt
            , xineScreens  = xinesc
            , wsOnScreen   = M.fromList $ map (\n -> (n,n)) [0..((length xinesc)-1)]
            , theRoot      = rootw
            , wmdelete     = wmdelt
            , wmprotocols  = wmprot
            , dimensions   = (displayWidth  dpy dflt, displayHeight dpy dflt)
            , workspace    = W.empty workspaces
            , layout       = Full
            , leftWidth    = defaultLeftWidth
            }

    xSetErrorHandler -- in C, I'm too lazy to write the binding

    -- setup initial X environment
    sync dpy False
    selectInput dpy rootw $  substructureRedirectMask
                         .|. substructureNotifyMask
                         .|. enterWindowMask
                         .|. leaveWindowMask
    grabKeys dpy rootw
    sync dpy False

    ws <- scan dpy rootw
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
    flip mapM_ (M.keys keys) $ \(mask,sym) -> do
         kc <- keysymToKeycode dpy sym
         mapM_ (grab kc) [mask, mask .|. numlockMask] -- note: no numlock
  where
    grab kc m = grabKey dpy kc m rootw True grabModeAsync grabModeAsync

-- | Restart xmonad by exec()'ing self. This doesn't save state and xmonad has
-- to be in PATH for this to work.
restart :: IO ()
restart = do prog <- getProgName
             args <- getArgs
             executeFile prog True args Nothing

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

-- run window manager command
handle (KeyEvent {event_type = t, state = m, keycode = code})
    | t == keyPress
    = withDisplay $ \dpy -> do
        s   <- io $ keycodeToKeysym dpy code 0
        whenJust (M.lookup (m,s) keys) id

-- manage a new window
handle (MapRequestEvent    {window = w}) = withDisplay $ \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    when (not (waOverrideRedirect wa)) $ manage w

-- window destroyed, unmanage it
handle (DestroyWindowEvent {window = w}) = do b <- isClient w; when b $ unmanage w

-- window gone, unmanage it
handle (UnmapEvent         {window = w}) = do b <- isClient w; when b $ unmanage w

-- set keyboard mapping
handle e@(MappingNotifyEvent {window = w}) = do
    let m = (request e, first_keycode e, count e)
    io $ refreshKeyboardMapping m
    when (request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

-- entered a normal window
handle e@(CrossingEvent {window = w, event_type = t})
    | t == enterNotify  && mode e == notifyNormal && detail e /= notifyInferior
    = do ws <- gets workspace
         if W.member w ws
            then setFocus w
            else do b <- isRoot w
                    when b setTopFocus

-- left a window, check if we need to focus root
handle e@(CrossingEvent {event_type = t})
    | t == leaveNotify
    = do rootw <- gets theRoot
         when (window e == rootw && not (same_screen e)) $ setFocus rootw

-- configure a window
handle e@(ConfigureRequestEvent {window = w}) = do
    dpy <- gets display
    ws  <- gets workspace

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
    ws2sc <- gets wsOnScreen
    xinesc <- gets xineScreens
    d <- gets display
    l <- gets layout
    ratio <- gets leftWidth
    let move w a b c e = io $ moveResizeWindow d w a b c e
    flip mapM_ (M.assocs ws2sc) $ \(n, scn) -> do
        let sc = xinesc !! scn
            sx = rect_x sc
            sy = rect_y sc
            sw = rect_width sc
            sh = rect_height sc
        case l of
            Full -> whenJust (W.peekStack n ws) $ \w -> do
                                                            move w sx sy sw sh
                                                            io $ raiseWindow d w
            Tile -> case W.index n ws of
                []    -> return ()
                [w]   -> do move w sx sy sw sh; io $ raiseWindow d w
                (w:s) -> do
                    let lw = floor $ fromIntegral sw * ratio
                        rw = sw - fromIntegral lw
                        rh = fromIntegral sh `div` fromIntegral (length s)
                    move w sx sy (fromIntegral lw) sh
                    zipWithM_ (\i a -> move a (sx + lw) (sy + i * rh) rw (fromIntegral rh)) [0..] s
                    whenJust (W.peek ws) (io . raiseWindow d) -- this is always Just
    whenJust (W.peek ws) setFocus

-- | switchLayout.  Switch to another layout scheme.
switchLayout :: X ()
switchLayout = do
    modify (\s -> s {layout = case layout s of
                                Full -> Tile
                                Tile -> Full })
    refresh

-- | changeWidth.  Change the width of the main window in tiling mode.
changeWidth :: Rational -> X ()
changeWidth delta = do
    modify (\s -> s {leftWidth = leftWidth s + delta})
    refresh

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WorkSpace -> WorkSpace) -> X ()
windows f = do
    modify $ \s -> s { workspace = f (workspace s) }
    refresh
    ws <- gets workspace
    trace (show ws) -- log state changes to stderr

-- | hide. Hide a window by moving it offscreen.
hide :: Window -> X ()
hide w = withDisplay $ \d -> do
    (sw,sh) <- gets dimensions
    io $ moveWindow d w (2*fromIntegral sw) (2*fromIntegral sh)

-- ---------------------------------------------------------------------
-- Window operations

-- | manage. Add a new window to be managed in the current workspace. Bring it into focus.
-- If the window is already under management, it is just raised.
--
-- When we start to manage a window, it gains focus.
--
manage :: Window -> X ()
manage w = do
    withDisplay $ \d -> io $ do
        selectInput d w $ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
        mapWindow d w
    setFocus w
    windows $ W.push w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
unmanage :: Window -> X ()
unmanage w = do
    windows $ W.delete w
    withServerX $ do
      setTopFocus
      withDisplay $ \d -> io (sync d False)
      -- TODO, everything operates on the current display, so wrap it up.

-- | Grab the X server (lock it) from the X monad
withServerX :: X () -> X ()
withServerX f = withDisplay $ \dpy -> do
    io $ grabServer dpy
    f
    io $ ungrabServer dpy

-- | Explicitly set the keyboard focus to the given window
setFocus :: Window -> X ()
setFocus w = withDisplay $ \d -> io $ setInputFocus d w revertToPointerRoot 0

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = do
    ws <- gets workspace
    case W.peek ws of
        Just new -> setFocus new
        Nothing  -> gets theRoot >>= setFocus

-- | raise. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
raise :: Ordering -> X ()
raise = windows . W.rotate

-- | promote.  Make the focused window the master window in its workspace
promote :: X ()
promote = windows (\w -> maybe w (\k -> W.promote k w) (W.peek w))

-- | Kill the currently focused client
kill :: X ()
kill = withDisplay $ \d -> do
    ws <- gets workspace
    whenJust (W.peek ws) $ \w -> do
        protocols <- io $ getWMProtocols d w
        wmdelt    <- gets wmdelete
        wmprot    <- gets wmprotocols
        if wmdelt `elem` protocols
            then io $ allocaXEvent $ \ev -> do
                    setEventType ev clientMessage
                    setClientMessageEvent ev w wmprot 32 wmdelt 0
                    sendEvent d w False noEventMask ev
            else io (killClient d w) >> return ()

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
    ws2sc <- gets wsOnScreen
    let m = W.current ws
    -- is the workspace we want to switch to currently visible?
    if M.member n ws2sc
        then windows $ W.view n
        else do 
            sc <- case M.lookup m ws2sc of
                Nothing -> do 
                    trace "Current workspace isn't visible! This should never happen!"
                    -- we don't know what screen to use, just use the first one.
                    return 0
                Just sc -> return sc
            modify $ \s -> s { wsOnScreen = M.insert n sc (M.filter (/=sc) ws2sc) }
            gets wsOnScreen >>= trace . show
            windows $ W.view n
            mapM_ hide (W.index m ws)
    setTopFocus
    where n = o-1

-- | True if window is under management by us
isClient :: Window -> X Bool
isClient w = liftM (W.member w) (gets workspace)

-- | screenWS. Returns the workspace currently visible on screen n
screenWS :: Int -> X Int
screenWS n = do
    ws2sc <- gets wsOnScreen
    -- FIXME: It's ugly to have to query this way. We need a different way to
    -- keep track of screen <-> workspace mappings.
    let ws = fmap fst $ find (\(_, scn) -> scn == (n-1)) (M.assocs ws2sc)
    return $ (fromMaybe 0 ws) + 1
