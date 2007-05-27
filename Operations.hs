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
import Data.List            (genericIndex, intersectBy)
import Data.Bits            ((.|.))
import qualified Data.Map as M

-- import System.Mem (performGC)
import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow

import Graphics.X11.Xlib
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras

-- ---------------------------------------------------------------------
-- Window manager operations

-- | manage. Add a new window to be managed in the current workspace.
-- Bring it into focus. If the window is already managed, nothing happens.
--
manage :: Window -> X ()
manage w = do
    withDisplay $ \d -> io $ do
        selectInput d w $ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
        mapWindow d w
        setWindowBorderWidth d w borderWidth
    windows $ W.insertUp w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
unmanage :: Window -> X ()
unmanage = windows . W.delete

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

-- | Kill the currently focused client. If we do kill it, we'll get a
-- delete notify back from X.
--
-- There are two ways to delete a window. Either just kill it, or if it
-- supports the delete protocol, send a delete event (e.g. firefox)
--
kill :: X ()
kill = withDisplay $ \d -> withFocused $ \w -> do
    XConf {wmdelete = wmdelt, wmprotocols = wmprot} <- ask
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
    old  <- gets windowset
    let new = f old
    modify (\s -> s { windowset = new })
    refresh

    -- We now go to some effort to compute the minimal set of windows to hide.
    -- The minimal set being only those windows which weren't previously hidden,
    -- which is the intersection of previously visible windows with those now hidden
    mapM_ hide . concatMap (integrate . W.stack) $
        intersectBy (\w x -> W.tag w == W.tag x)
            (map W.workspace $ W.current old : W.visible old)
            (W.hidden new)

    clearEnterEvents

    -- TODO: move this into StackSet.  This isn't exactly the usual integrate.
 where integrate W.Empty        = []
       integrate (W.Node x l r) = x : l ++ r

-- | hide. Hide a window by moving it off screen.
hide :: Window -> X ()
hide w = withDisplay $ \d -> do
    (sw,sh) <- gets dimensions
    io $ moveWindow d w sw sh

-- | refresh. Render the currently visible workspaces, as determined by
-- the StackSet. Also, set focus to the focused window.
--
-- This is our 'view' operation (MVC), in that it pretty prints our model
-- with X calls.
--
refresh :: X ()
refresh = do
    XState { windowset = ws, layouts = fls, xineScreens = xinesc } <- get
    d <- asks display

    -- for each workspace, layout the currently visible workspaces
    (`mapM_` (W.current ws : W.visible ws)) $ \w -> do
        let n      = W.tag (W.workspace w)
            this   = W.view n ws
            Just l = fmap fst $ M.lookup n fls
        -- now tile the windows on this workspace
        rs <- doLayout l (genericIndex xinesc (W.screen w)) (W.index this)
        mapM_ (\(win,rect) -> io (tileWindow d win rect)) rs

        -- and raise the focused window if there is one.
        whenJust (W.peek this) $ io . raiseWindow d

    setTopFocus
    clearEnterEvents
--  io performGC -- really helps 

-- | clearEnterEvents.  Remove all window entry events from the event queue.
clearEnterEvents :: X ()
clearEnterEvents = withDisplay $ \d -> io $ do
    sync d False
    allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d enterWindowMask p
        when more again -- beautiful

-- | tileWindow. Moves and resizes w such that it fits inside the given
-- rectangle, including its border.
tileWindow :: Display -> Window -> Rectangle -> IO ()
tileWindow d w r = do
    bw <- (fromIntegral . wa_border_width) `liftM` getWindowAttributes d w
    moveResizeWindow d w (rect_x r) (rect_y r)
                         (rect_width  r - bw*2) (rect_height r - bw*2)

-- ---------------------------------------------------------------------

-- | rescreen.  The screen configuration may have changed (due to
-- xrandr), update the state and refresh the screen.
rescreen :: X ()
rescreen = do
    xinesc <- withDisplay (io . getScreenInfo)

    -- TODO: This stuff is necessary because Xlib apparently caches screen
    -- width/height.  Find a better solution later.  I hate Xlib.
    let sx = maximum $ map (\r -> rect_x r + fromIntegral (rect_width  r)) xinesc
        sy = maximum $ map (\r -> rect_y r + fromIntegral (rect_height r)) xinesc

    modify (\s -> s { xineScreens = xinesc, dimensions = (sx, sy) })

    windows $ \ws@(W.StackSet { W.current = v, W.visible = vs, W.hidden = hs }) ->
        let (x:xs, ys) = splitAt (length xinesc) $ map W.workspace (v:vs) ++ hs
        in  ws { W.current = W.Screen x 0
               , W.visible = zipWith W.Screen xs [1 ..]
               , W.hidden  = ys }

-- ---------------------------------------------------------------------

buttonsToGrab :: [Button]
buttonsToGrab = [button1, button2, button3]

-- | setButtonGrab. Tell whether or not to intercept clicks on a given window
setButtonGrab :: Bool -> Window -> X ()
setButtonGrab grab w = withDisplay $ \d -> io $ (`mapM_` buttonsToGrab) $ \b ->
    if grab then grabButton d b anyModifier w False (buttonPressMask .|. buttonReleaseMask)
                   grabModeAsync grabModeSync none none
            else ungrabButton d b anyModifier w

-- ---------------------------------------------------------------------
-- Setting keyboard focus

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = withWorkspace $ maybe (setFocusX =<< asks theRoot) setFocusX . W.peek

-- | Set focus explicitly to window 'w' if it is managed by us, or root.
focus :: Window -> X ()
focus w = withWorkspace $ \s -> do
    if W.member w s then do modify $ \st -> st { windowset = W.focusWindow w s } -- avoid 'refresh'
                            setFocusX w
                    else whenX (isRoot w) $ setFocusX w

-- | Call X to set the keyboard focus details.
setFocusX :: Window -> X ()
setFocusX w = withWorkspace $ \ws -> do
    XConf { display = dpy , normalBorder = nbc, focusedBorder = fbc } <- ask

    -- clear mouse button grab and border on other windows
    (`mapM_` (W.current ws : W.visible ws)) $ \wk -> do
        (`mapM_` (W.index (W.view (W.tag (W.workspace wk)) ws))) $ \otherw -> do
            setButtonGrab True otherw
            io $ setWindowBorder dpy otherw (color_pixel nbc)

    io $ do setInputFocus dpy w revertToPointerRoot 0
            raiseWindow dpy w
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
--   wide mode
-- 
-- The latter algorithms support the following operations:
--
--      Shrink
--      Expand
--

data Resize = Shrink | Expand deriving Typeable
instance Message Resize

data IncMasterN = IncMasterN Int deriving Typeable
instance Message IncMasterN

full :: Layout
full = Layout { doLayout     = \sc ws -> return [ (w,sc) | w <- ws ]
              , modifyLayout = const Nothing } -- no changes

tall, wide :: Int -> Rational -> Rational -> Layout
wide nmaster delta frac = mirrorLayout (tall nmaster delta frac)

tall nmaster delta frac =
    Layout { doLayout     = \r -> return . ap zip (tile frac r nmaster . length)
           , modifyLayout = \m -> fmap resize     (fromMessage m) `mplus`
                                  fmap incmastern (fromMessage m) }

    where resize Shrink = tall nmaster delta (frac-delta)
          resize Expand = tall nmaster delta (frac+delta)
          incmastern (IncMasterN d) = tall (max 1 (nmaster+d)) delta frac

-- | Mirror a rectangle
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = (Rectangle ry rx rh rw)

-- | Mirror a layout
mirrorLayout :: Layout -> Layout
mirrorLayout (Layout { doLayout = dl, modifyLayout = ml }) =
              Layout { doLayout = \sc w -> map (second mirrorRect) `fmap` dl (mirrorRect sc) w
                     , modifyLayout = fmap mirrorLayout . ml }

-- | tile.  Compute the positions for windows in our default tiling modes
-- Tiling algorithms in the core should satisify the constraint that
--
--  * no windows overlap
--  * no gaps exist between windows.
--
tile :: Rational -> Rectangle -> Int -> Int -> [Rectangle]
tile f r nmaster n | n <= nmaster = splitVertically n r
                   | otherwise    = splitVertically nmaster r1 ++ splitVertically (n-nmaster) r2
    where (r1,r2) = splitHorizontallyBy f r

splitVertically, splitHorizontally :: Int -> Rectangle -> [Rectangle]
splitVertically n r | n < 2 = [r]
splitVertically n (Rectangle sx sy sw sh) = Rectangle sx sy sw smallh :
    splitVertically (n-1) (Rectangle sx (sy+fromIntegral smallh) sw (sh-smallh))
    where smallh = sh `div` fromIntegral n
splitHorizontally n r = map mirrorRect $ splitVertically n $ mirrorRect r

splitHorizontallyBy, splitVerticallyBy :: Rational -> Rectangle -> (Rectangle, Rectangle)
splitHorizontallyBy f (Rectangle sx sy sw sh) =
    (Rectangle sx sy leftw sh, Rectangle (sx + fromIntegral leftw) sy (sw-fromIntegral leftw) sh)
        where leftw = floor $ fromIntegral sw * f
splitVerticallyBy f r = (\(a,b)->(mirrorRect a,mirrorRect b)) $ splitHorizontallyBy f $ mirrorRect r

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
screenWorkspace sc = withWorkspace $ return . fromMaybe 0 . W.lookupWorkspace sc

-- | Apply an X operation to the currently focused window, if there is one.
withFocused :: (Window -> X ()) -> X ()
withFocused f = withWorkspace $ \w -> whenJust (W.peek w) f

-- | True if window is under management by us
isClient :: Window -> X Bool
isClient w = withWorkspace $ return . W.member w
