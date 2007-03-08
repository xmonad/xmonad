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
import qualified Data.Sequence as S
import qualified Data.Foldable as F
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
    , ((mod1Mask .|. shiftMask, xK_c     ), kill)
    , ((mod1Mask .|. shiftMask, xK_q     ), io $ exitWith ExitSuccess)

    , ((mod1Mask,               xK_1     ), view 1)
    , ((mod1Mask,               xK_2     ), view 2)
    , ((mod1Mask,               xK_3     ), view 3)
    , ((mod1Mask,               xK_4     ), view 4)
    , ((mod1Mask,               xK_5     ), view 5)

    , ((mod1Mask .|. shiftMask, xK_1     ), tag 1)
    , ((mod1Mask .|. shiftMask, xK_2     ), tag 2)
    , ((mod1Mask .|. shiftMask, xK_3     ), tag 3)
    , ((mod1Mask .|. shiftMask, xK_4     ), tag 4)
    , ((mod1Mask .|. shiftMask, xK_5     ), tag 5)

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
            , workspace    = (0,S.fromList (replicate 5 [])) -- 5 empty workspaces
            }

    runW initState $ do
        r <- io $ rootWindow dpy dflt
        io $ do selectInput dpy r (substructureRedirectMask .|. substructureNotifyMask)
                sync dpy False
        registerKeys dpy r
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
    | t == keyPress = do
        dpy <- gets display
        s   <- io $ keycodeToKeysym dpy code 0
        case M.lookup (m,s) keys of
            Nothing -> return ()
            Just a  -> a

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
    (n,wks) <- gets workspace
    let ws = wks `S.index` n
    case ws of
        []    -> return ()  -- do nothing. hmm. so no empty workspaces?
                            -- we really need to hide all non-visible windows
                            -- ones on other screens
        (w:_) -> do
            d  <- gets display
            sw <- liftM fromIntegral (gets screenWidth)
            sh <- liftM fromIntegral (gets screenHeight)
            io $ do moveResizeWindow d w 0 0 sw sh -- fullscreen
                    raiseWindow d w

-- | Modify the current window list with a pure funtion, and refresh
withWindows :: (Windows -> Windows) -> W ()
withWindows f = do
    modifyWindows f
    refresh

-- | manage. Add a new window to be managed in the current workspace. Bring it into focus.
manage :: Window -> W ()
manage w = do
    d  <- gets display
    io $ mapWindow d w
    withWindows (nub . (w :))

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
unmanage :: Window -> W ()
unmanage w = do
    (_,wks) <- gets workspace
    mapM_ rm (F.toList wks)
  where
    rm ws = when (w `elem` ws) $ do
                dpy     <- gets display
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
    dpy     <- gets display
    (n,wks) <- gets workspace
    let ws = wks `S.index` n
    case ws of
        []    -> return ()
        (w:_) -> do
        --  if(isprotodel(sel))
        --      sendevent(sel->win, wmatom[WMProtocols], wmatom[WMDelete]);
            io $ killClient dpy w -- ignoring result
            return ()

-- | tag. associate a window with a new workspace
tag :: Int -> W ()
tag n = do
    let new = n-1
    (old,wks) <- gets workspace
    when (new /= old && new >= 0 && new < S.length wks) $ do
        let this = wks `S.index` old
        if null this
            then return ()  -- no client to retag
            else do let (t:_) = this
                    modifyWorkspaces $ \(i,w) ->
                         let w'  = S.adjust tail old w
                             w'' = S.adjust (t:) new w' in (i,w'')
                    hideWindows [t]
                    refresh

-- | Change the current workspace to workspce at offset 'n-1'.
view :: Int -> W ()
view n = do
    let new = n-1
    (old,wks) <- gets workspace
    when (new /= old && new >= 0 && new < S.length wks) $ do
        modifyWorkspaces $ \_ -> (new,wks)
        hideWindows (wks `S.index` old)
        showWindows (wks `S.index` new)
        refresh

-- | Hide a list of windows by moving them offscreen.
hideWindows :: Windows -> W ()
hideWindows ws = do
    dpy     <- gets display
    sw      <- liftM fromIntegral (gets screenWidth)
    sh      <- liftM fromIntegral (gets screenHeight)
    forM_ ws $ \w -> io $ moveWindow dpy w (2*sw) (2*sh)

-- | Expose a list of windows, moving them on screen
showWindows :: Windows -> W ()
showWindows ws = do
    dpy     <- gets display
    forM_ ws $ \w -> io $ moveWindow dpy w 0 0
