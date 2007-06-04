{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Operations.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  unstable
-- Portability :  not portable, mtl, posix
--
-----------------------------------------------------------------------------

module Operations where

import XMonad
import qualified StackSet as W
import {-# SOURCE #-} Config (borderWidth)

import Data.Maybe
import Data.List            (genericIndex, intersectBy, partition, delete)
import Data.Bits            ((.|.))
import Data.Ratio
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***), second)

import System.IO
import Graphics.X11.Xlib
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras

-- ---------------------------------------------------------------------
-- Window manager operations

-- | manage. Add a new window to be managed in the current workspace.
-- Bring it into focus. If the window is already managed, nothing happens.
--
manage :: Window -> X ()
manage w = withDisplay $ \d -> do
    io $ selectInput d w $ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
    io $ mapWindow d w
    io $ setWindowBorderWidth d w borderWidth

    -- FIXME: This is pretty awkward. We can't can't let "refresh" happen
    -- before the call to float, because that will resize the window and
    -- lose the default sizing.

    isTransient <- isJust `liftM` io (getTransientForHint d w)
    if isTransient
        then do modify $ \s -> s { windowset = W.insertUp w (windowset s) }
                float w -- ^^ now go the refresh.
        else windows $ W.insertUp w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
--
-- FIXME: clearFloating should be taken care of in W.delete, but if we do it
-- there, floating status is lost when moving windows between workspaces,
-- because W.shift calls W.delete.
unmanage :: Window -> X ()
unmanage w = setWMState w 0{-withdrawn-} >> windows (W.sink w . W.delete w)

-- | focus. focus window up or down. or swap various windows.
focusUp, focusDown, swapUp, swapDown, swapMaster :: X ()
focusUp    = windows W.focusUp
focusDown  = windows W.focusDown
swapUp     = windows W.swapUp
swapDown   = windows W.swapDown
swapMaster = windows W.swapMaster

-- | shift. Move a window to a new workspace, 0 indexed.
shift :: WorkspaceId -> X ()
shift n = withFocused hide >> windows (W.shift n)
    -- refresh will raise it if we didn't need to move it.

-- | view. Change the current workspace to workspace at offset n (0 indexed).
view :: WorkspaceId -> X ()
view = windows . W.view

-- | Modify the size of the status gap at the top of the current screen
-- Taking a function giving the current screen, and current geometry.
modifyGap :: (Int -> (Int,Int,Int,Int) -> (Int,Int,Int,Int)) -> X ()
modifyGap f = do
    XState { windowset = ws, statusGaps = gaps } <- get
    let n       = fromIntegral $ W.screen (W.current ws)
        (a,i:b) = splitAt n gaps
    modify $ \s -> s { statusGaps = a ++ f n i : b }
    refresh

-- | Kill the currently focused client. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
--
kill :: X ()
kill = withDisplay $ \d -> withFocused $ \w -> do
    wmdelt <- atom_WM_DELETE_WINDOW  ;  wmprot <- atom_WM_PROTOCOLS

    protocols <- io $ getWMProtocols d w
    io $ if wmdelt `elem` protocols
        then allocaXEvent $ \ev -> do
                setEventType ev clientMessage
                setClientMessageEvent ev w wmprot 32 wmdelt 0
                sendEvent d w False noEventMask ev
        else killClient d w >> return ()

-- ---------------------------------------------------------------------
-- Managing windows

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    XState { windowset = old, layouts = fls, xineScreens = xinesc, statusGaps = gaps } <- get
    let ws = f old
    modify (\s -> s { windowset = ws })
    d <- asks display

    -- for each workspace, layout the currently visible workspaces
    forM_ (W.current ws : W.visible ws) $ \w -> do
        let n      = W.tag (W.workspace w)
            this   = W.view n ws
            Just l = fmap fst $ M.lookup n fls
            (flt, tiled) = partition (flip M.member (W.floating ws)) (W.index this)
            (Rectangle sx sy sw sh) = genericIndex xinesc (W.screen w)
            (gt,gb,gl,gr)           = genericIndex gaps   (W.screen w)

        -- just the tiled windows:
        -- now tile the windows on this workspace, modified by the gap
        rs <- doLayout l (Rectangle
                (sx + fromIntegral gl)        (sy + fromIntegral gt)
                (sw - fromIntegral (gl + gr)) (sh - fromIntegral (gt + gb))) tiled
        mapM_ (\(win,rect) -> tileWindow win rect) rs

        -- now the floating windows:
        -- move/resize the floating windows, if there are any
        forM_ flt $ \fw -> whenJust (M.lookup fw (W.floating ws)) $
          \(W.RationalRect rx ry rw rh) -> do
            tileWindow fw $ Rectangle
                (sx + floor (toRational sw*rx)) (sy + floor (toRational sh*ry))
                (floor (toRational sw*rw)) (floor (toRational sh*rh))

        -- TODO seems fishy?
        -- Urgh. This is required because the fullscreen layout assumes that
        -- the focused window will be raised. Hmm. This is a reordering.

        -- This really doesn't work with fullscreen mode, where 
        -- focus is used to find the raised window. moving the floating
        -- layer will move focus there, so we now have forgotten the
        -- window on the top of the fullscreen
        --
        -- I think the solution must be to track the floating layer separately
        -- in its own zipper, on each workspace. And from there to
        -- handle pushing between the two.
        --
        let tiled' = case W.peek this of
                        Just x | x `elem` tiled -> x : delete x tiled
                        _ -> tiled

        io $ restackWindows d (flt ++ tiled')

    setTopFocus
    -- withWindowSet (io . hPrint stderr) -- logging state changes!
    -- io performGC -- really helps 

    -- We now go to some effort to compute the minimal set of windows to hide.
    -- The minimal set being only those windows which weren't previously hidden,
    -- which is the intersection of previously visible windows with those now hidden
    mapM_ hide . concatMap (W.integrate . W.stack) $
        intersectBy (\w x -> W.tag w == W.tag x)
            (map W.workspace $ W.current old : W.visible old)
            (W.hidden ws)

    clearEnterEvents

-- | setWMState.  set the WM_STATE property
setWMState :: Window -> Int -> X ()
setWMState w v = withDisplay $ \dpy -> do
    a <- atom_WM_STATE
    io $ changeProperty32 dpy w a a propModeReplace [fromIntegral v, fromIntegral none]

-- | hide. Hide a window by unmapping it.
hide :: Window -> X ()
hide w = withDisplay $ \d -> do
    io $ unmapWindow d w
    setWMState w 3 --iconic

-- | refresh. Render the currently visible workspaces, as determined by
-- the StackSet. Also, set focus to the focused window.
--
-- This is our 'view' operation (MVC), in that it pretty prints our model
-- with X calls.
--
refresh :: X ()
refresh = windows id

-- | clearEnterEvents.  Remove all window entry events from the event queue.
clearEnterEvents :: X ()
clearEnterEvents = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d enterWindowMask p
        when more again -- beautiful

-- | tileWindow. Moves and resizes w such that it fits inside the given
-- rectangle, including its border.
tileWindow :: Window -> Rectangle -> X ()
tileWindow w r = withDisplay $ \d -> do
    bw <- (fromIntegral . wa_border_width) `liftM` io (getWindowAttributes d w)
    io $ moveResizeWindow d w (rect_x r) (rect_y r)
                              (rect_width  r - bw*2) (rect_height r - bw*2)
    -- this is harmless if the window was already visible
    setWMState w 1 --normal
    io $ mapWindow d w

-- ---------------------------------------------------------------------

-- | rescreen.  The screen configuration may have changed (due to
-- xrandr), update the state and refresh the screen, and reset the gap.
rescreen :: X ()
rescreen = do
    xinesc <- withDisplay (io . getScreenInfo)

    modify (\s -> s { xineScreens = xinesc
                    , statusGaps  = take (length xinesc) $ (statusGaps s) ++ repeat (0,0,0,0) })

    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let (x:xs, ys) = splitAt (length xinesc) $ map W.workspace (v:vs) ++ hs
        in  ws { W.current = W.Screen x 0
               , W.visible = zipWith W.Screen xs [1 ..]
               , W.hidden  = ys }

-- ---------------------------------------------------------------------

-- | setButtonGrab. Tell whether or not to intercept clicks on a given window
setButtonGrab :: Bool -> Window -> X ()
setButtonGrab grab w = withDisplay $ \d -> io $
    if grab
        then forM_ [button1, button2, button3] $ \b ->
            grabButton d b anyModifier w False buttonPressMask
                       grabModeAsync grabModeSync none none
        else ungrabButton d anyButton anyModifier w

-- ---------------------------------------------------------------------
-- Setting keyboard focus

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = withWindowSet $ maybe (setFocusX =<< asks theRoot) setFocusX . W.peek

-- | Set focus explicitly to window 'w' if it is managed by us, or root.
-- This happens if X notices we've moved the mouse (and perhaps moved
-- the mouse to a new screen).
focus :: Window -> X ()
focus w = withWindowSet $ \s -> do
    if W.member w s then windows (W.focusWindow w)
                    else whenX (isRoot w) $ setFocusX w

-- | Call X to set the keyboard focus details.
setFocusX :: Window -> X ()
setFocusX w = withWindowSet $ \ws -> do
    XConf { display = dpy , normalBorder = nbc, focusedBorder = fbc } <- ask

    -- clear mouse button grab and border on other windows
    forM_ (W.current ws : W.visible ws) $ \wk -> do
        forM_ (W.index (W.view (W.tag (W.workspace wk)) ws)) $ \otherw -> do
            setButtonGrab True otherw
            io $ setWindowBorder dpy otherw (color_pixel nbc)

    whenX (not `liftM` isRoot w) $ do
        io $ do setInputFocus dpy w revertToPointerRoot 0
                -- raiseWindow dpy w
        setButtonGrab False w
        io $ setWindowBorder dpy w (color_pixel fbc)

-- ---------------------------------------------------------------------
-- Managing layout

-- | switchLayout.  Switch to another layout scheme.  Switches the
-- layout of the current workspace. By convention, a window set as
-- master in Tall mode remains as master in Wide mode. When switching
-- from full screen to a tiling mode, the currently focused window
-- becomes a master. When switching back , the focused window is
-- uppermost.
--
switchLayout :: X ()
switchLayout = layout (\(x, xs) -> let xs' = xs ++ [x] in (head xs', tail xs'))

-- | Throw an (extensible) message value to the current Layout scheme,
-- possibly modifying how we layout the windows, then refresh.
--
-- TODO, this will refresh on Nothing.
--
sendMessage :: Message a => a -> X ()
sendMessage a = layout $ \x@(l, ls) -> maybe x (flip (,) ls) (modifyLayout l (SomeMessage a))

--
-- Builtin layout algorithms:
--
--   fullscreen mode
--   tall mode
--   wide mode (a mirror of tall mode)
-- 
-- The latter algorithms support the following operations:
--
--      Shrink
--      Expand
--

data Resize     = Shrink | Expand   deriving Typeable
data IncMasterN = IncMasterN Int   deriving Typeable
instance Message Resize
instance Message IncMasterN

-- simple fullscreen mode, just render all windows fullscreen.
-- a plea for tuple sections: map . (,sc)
full :: Layout
full = Layout { doLayout     = \sc ws -> return [ (w,sc) | w <- ws ]
              , modifyLayout = const Nothing } -- no changes

--
-- The tiling mode of xmonad, and its operations.
--
tall :: Int -> Rational -> Rational -> Layout
tall nmaster delta frac =
    Layout { doLayout     = \r -> return . ap zip (tile frac r nmaster . length)
           , modifyLayout = \m -> fmap resize     (fromMessage m) `mplus`
                                  fmap incmastern (fromMessage m) }

    where resize Shrink = tall nmaster delta (frac-delta)
          resize Expand = tall nmaster delta (frac+delta)
          incmastern (IncMasterN d) = tall (max 0 (nmaster+d)) delta frac

-- | Mirror a rectangle
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = (Rectangle ry rx rh rw)

-- | Mirror a layout, compute its 90 degree rotated form.
mirror :: Layout -> Layout
mirror (Layout { doLayout = dl, modifyLayout = ml }) =
    Layout { doLayout     = \sc w -> map (second mirrorRect) `fmap` dl (mirrorRect sc) w
           , modifyLayout = fmap mirror . ml }

-- | tile.  Compute the positions for windows using the default 2 pane tiling algorithm.
--
-- The screen is divided (currently) into two panes. all clients are
-- then partioned between these two panes. one pane, the `master', by
-- convention has the least number of windows in it (by default, 1). 
-- the variable `nmaster' controls how many windows are rendered in the
-- master pane. 
--
-- `delta' specifies the ratio of the screen to resize by.
--
-- 'frac' specifies what proportion of the screen to devote to the
-- master area.
-- 
tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n = if n <= nmaster || nmaster == 0
    then splitVertically n r
    else splitVertically nmaster r1 ++ splitVertically (n-nmaster) r2 -- two columns
  where (r1,r2) = splitHorizontallyBy f r

--
-- Divide the screen vertically into n subrectangles
--
splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
  where smallh = sh `div` fromIntegral n --hmm, this is a fold or map.

splitHorizontally n = map mirrorRect . splitVertically n . mirrorRect

-- Divide the screen into two rectangles, using a rational to specify the ratio
splitHorizontallyBy, splitVerticallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    ( Rectangle sx sy leftw sh
    , Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
  where leftw = floor $ fromIntegral sw * f

splitVerticallyBy f = (mirrorRect *** mirrorRect) . splitHorizontallyBy f . mirrorRect

------------------------------------------------------------------------

-- | layout. Modify the current workspace's layout with a pure
-- function and refresh.
layout :: ((Layout, [Layout]) -> (Layout, [Layout])) -> X ()
layout f = do
    modify $ \s ->
        let n          = W.tag . W.workspace . W.current . windowset $ s
            (Just fl)  = M.lookup n $ layouts s
        in s { layouts = M.insert n (f fl) (layouts s) }
    refresh

------------------------------------------------------------------------
-- Utilities

-- | Return workspace visible on screen 'sc', or 0.
screenWorkspace :: ScreenId -> X WorkspaceId
screenWorkspace sc = withWindowSet $ return . fromMaybe 0 . W.lookupWorkspace sc

-- | Apply an X operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWindowSet $ \w -> whenJust (W.peek w) f

-- | True if window is under management by us
isClient :: Window -> X Bool
isClient w = withWindowSet $ return . W.member w

------------------------------------------------------------------------
-- | Floating layer support

-- | Make a floating window tiled
sink :: Window -> X ()
sink = windows . W.sink

-- | Make a tiled window floating, using its suggested rectangle
float :: Window -> X ()
float w = withDisplay $ \d -> do
    xinesc <- gets xineScreens
    sc     <- (genericIndex xinesc . W.screen . W.current) `liftM` gets windowset
    wa     <- io $ getWindowAttributes d w
    let bw = fi . wa_border_width $ wa
    windows $ W.float w
        (W.RationalRect ((fi (wa_x wa) - fi (rect_x sc)) % fi (rect_width sc))
                        ((fi (wa_y wa) - fi (rect_y sc)) % fi (rect_height sc))
                        (fi (wa_width  wa + bw*2) % fi (rect_width sc))
                        (fi (wa_height wa + bw*2) % fi (rect_height sc)))
  where fi x = fromIntegral x

-- | Toggle floating bit
--
-- TODO not useful unless we remember the original size
--
-- toggleFloating :: Window -> X ()
-- toggleFloating w = gets windowset >>= \ws -> if M.member w (W.floating ws) then sink w else float w

------------------------------------------------------------------------
-- mouse handling

-- | Accumulate mouse motion events
mouseDrag :: (XMotionEvent -> IO ()) -> X ()
mouseDrag f = do
    XConf { theRoot = root, display = d } <- ask
    io $ grabPointer d root False (buttonReleaseMask .|. pointerMotionMask)
            grabModeAsync grabModeAsync none none currentTime
    io $ allocaXEvent $ \p -> fix $ \again -> do -- event loop
        maskEvent d (buttonReleaseMask .|. pointerMotionMask) p
        et <- get_EventType p
        when (et == motionNotify) $ get_MotionEvent p >>= f >> again
    io $ ungrabPointer d currentTime

mouseMoveWindow :: Window -> X ()
mouseMoveWindow w = withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    (_, _, _, ox, oy, _, _, _) <- io $ queryPointer d w
    mouseDrag $ \(_, _, _, ex, ey, _, _, _, _, _) ->
       moveWindow d w (fromIntegral (fromIntegral (wa_x wa) + (ex - ox)))
                      (fromIntegral (fromIntegral (wa_y wa) + (ey - oy)))
    float w

mouseResizeWindow :: Window -> X ()
mouseResizeWindow w = withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa)) (fromIntegral (wa_height wa))
    mouseDrag $ \(_, _, _, ex, ey, _, _, _, _, _) ->
        resizeWindow d w (fromIntegral (max 1 (ex - fromIntegral (wa_x wa))))
                         (fromIntegral (max 1 (ey - fromIntegral (wa_y wa))))
    float w
--

-- generic handler, but too complex:
--
-- mouseModifyWindow f g w = withDisplay $ \d -> do
--     io $ raiseWindow d w
--     wa <- io $ getWindowAttributes d w
--     x  <- f d w wa
--     mouseDrag $ \(_,_,_,ex,ey,_,_,_,_,_) -> g x ex ey d w wa
--     float w
