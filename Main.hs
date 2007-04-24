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

import Data.Bits
import qualified Data.Map as M

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras
import Graphics.X11.Xinerama

import Control.Monad.State

import qualified StackSet as W

import XMonad
import Operations
import Config

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
            { display       = dpy
            , xineScreens   = xinesc
            , theRoot       = rootw
            , wmdelete      = wmdelt
            , wmprotocols   = wmprot
            -- fromIntegral needed for X11 versions that use Int instead of CInt.
            , dimensions    = (fromIntegral (displayWidth dpy dflt),
                               fromIntegral (displayHeight dpy dflt))
            , workspace     = W.empty workspaces (length xinesc)
            , defaultLayoutDesc = startingLayoutDesc
            , layoutDescs   = M.empty
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
              return $ not (wa_override_redirect wa)
                     && wa_map_state wa == waIsViewable

-- | Grab the keys back
grabKeys :: Display -> Window -> IO ()
grabKeys dpy rootw = do
    ungrabKey dpy '\0' {-AnyKey-} anyModifier rootw
    flip mapM_ (M.keys keys) $ \(mask,sym) -> do
         kc <- keysymToKeycode dpy sym
         mapM_ (grab kc) [mask, mask .|. numlockMask] -- note: no numlock
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

-- run window manager command
handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress
    = withDisplay $ \dpy -> do
        s   <- io $ keycodeToKeysym dpy code 0
        whenJust (M.lookup (complement numlockMask .&. m,s) keys) id

-- manage a new window
handle (MapRequestEvent    {ev_window = w}) = withDisplay $ \dpy -> do
    wa <- io $ getWindowAttributes dpy w -- ignore override windows
    when (not (wa_override_redirect wa)) $ manage w

-- window destroyed, unmanage it
handle (DestroyWindowEvent {ev_window = w}) = do b <- isClient w; when b $ unmanage w

-- window gone, unmanage it
handle (UnmapEvent         {ev_window = w}) = do b <- isClient w; when b $ unmanage w

-- set keyboard mapping
handle e@(MappingNotifyEvent {ev_window = w}) = do
    -- this fromIntegral is only necessary with the old X11 version that uses
    -- Int instead of CInt.  TODO delete it when there is a new release of X11
    let m = (ev_request e, ev_first_keycode e, fromIntegral $ ev_count e)
    withDisplay $ \d -> io $ refreshKeyboardMapping d m
    when (ev_request e == mappingKeyboard) $ withDisplay $ io . flip grabKeys w

-- click on an unfocussed window
handle (ButtonEvent {ev_window = w, ev_event_type = t})
    | t == buttonPress
    = safeFocus w

-- entered a normal window
handle e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify  && ev_mode e == notifyNormal && ev_detail e /= notifyInferior
    = safeFocus w

-- left a window, check if we need to focus root
handle e@(CrossingEvent {ev_event_type = t})
    | t == leaveNotify
    = do rootw <- gets theRoot
         when (ev_window e == rootw && not (ev_same_screen e)) $ setFocus rootw

-- configure a window
handle e@(ConfigureRequestEvent {ev_window = w}) = do
    XState { display = dpy, workspace = ws } <- get

    when (W.member w ws) $ -- already managed, reconfigure (see client:configure()
        trace ("Reconfigure already managed window: " ++ show w)

    io $ configureWindow dpy (ev_window e) (ev_value_mask e) $ WindowChanges
        { wc_x            = ev_x e
        , wc_y            = ev_y e
        , wc_width        = ev_width e
        , wc_height       = ev_height e
        , wc_border_width = ev_border_width e
        , wc_sibling      = ev_above e
        -- this fromIntegral is only necessary with the old X11 version that uses
        -- Int instead of CInt.  TODO delete it when there is a new release of X11
        , wc_stack_mode   = fromIntegral $ ev_detail e
        }

    io $ sync dpy False

handle e = trace (eventName e) -- ignoring
