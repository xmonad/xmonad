{-# OPTIONS -fno-warn-orphans -fglasgow-exts #-}
-- \^^ deriving Typeable
-- --------------------------------------------------------------------------
-- |
-- Module      :  Operations.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- Operations.
--
-----------------------------------------------------------------------------

module Operations where

import XMonad
import qualified StackSet as W
import {-# SOURCE #-} Config (borderWidth,logHook,numlockMask)

import Data.Maybe
import Data.List            (genericIndex, nub, (\\))
import Data.Bits            ((.|.), (.&.), complement)
import Data.Ratio
import qualified Data.Map as M
import qualified Data.Set as S

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow ((***), second)

import System.IO
import Graphics.X11.Xlib
import Graphics.X11.Xinerama (getScreenInfo)
import Graphics.X11.Xlib.Extras

import qualified Data.Traversable as T

-- ---------------------------------------------------------------------
-- |
-- Window manager operations
-- manage. Add a new window to be managed in the current workspace.
-- Bring it into focus.
--
-- Whether the window is already managed, or not, it is mapped, has its
-- border set, and its event mask set.
--
manage :: Window -> X ()
manage w = withDisplay $ \d -> do
    setInitialProperties w >> reveal w

    -- FIXME: This is pretty awkward. We can't can't let "refresh" happen
    -- before the call to float, because that will resize the window and
    -- lose the default sizing.

    sh <- io $ getWMNormalHints d w
    let isFixedSize = sh_min_size sh /= Nothing && sh_min_size sh == sh_max_size sh
    isTransient <- isJust `liftM` io (getTransientForHint d w)
    if isFixedSize || isTransient
        then do modify $ \s -> s { windowset = W.insertUp w (windowset s) }
                float w -- \^^ now go the refresh.
        else windows $ W.insertUp w

-- | unmanage. A window no longer exists, remove it from the window
-- list, on whatever workspace it is.
--
-- FIXME: clearFloating should be taken care of in W.delete, but if we do it
-- there, floating status is lost when moving windows between workspaces,
-- because W.shift calls W.delete.
--
-- should also unmap?
--
unmanage :: Window -> X ()
unmanage w = do
    windows (W.sink w . W.delete w)
    setWMState w 0 {-withdrawn-}
    modify (\s -> s {mapped = S.delete w (mapped s), waitingUnmap = M.delete w (waitingUnmap s)})

-- | focus. focus window up or down. or swap various windows.
focusUp, focusDown, swapUp, swapDown, swapMaster :: X ()
focusUp    = windows W.focusUp
focusDown  = windows W.focusDown
swapUp     = windows W.swapUp
swapDown   = windows W.swapDown
swapMaster = windows W.swapMaster

-- | shift. Move a window to a new workspace, 0 indexed.
shift :: WorkspaceId -> X ()
shift n = windows (W.shift n)

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

data UnDoLayout = UnDoLayout deriving ( Typeable, Eq )
instance Message UnDoLayout

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    -- Notify visible layouts to remove decorations etc
    -- We cannot use sendMessage because this must not call refresh ever,
    -- and must be called on all visible workspaces.
    broadcastMessage UnDoLayout
    XState { windowset = old, layouts = fls, xineScreens = xinesc, statusGaps = gaps } <- get
    let oldvisible = concatMap (W.integrate' . W.stack . W.workspace) $ W.current old : W.visible old
        ws = f old
    modify (\s -> s { windowset = ws })
    d <- asks display

    -- for each workspace, layout the currently visible workspaces
    visible <- fmap concat $ forM (W.current ws : W.visible ws) $ \w -> do
        let n      = W.tag (W.workspace w)
            this   = W.view n ws
            Just l = fmap fst $ M.lookup n fls
            flt = filter (flip M.member (W.floating ws)) (W.index this)
            tiled = (W.stack . W.workspace . W.current $ this)
                    >>= W.filter (not . flip M.member (W.floating ws))
            (Rectangle sx sy sw sh) = genericIndex xinesc (W.screen w)
            (gt,gb,gl,gr)           = genericIndex gaps   (W.screen w)
            viewrect = Rectangle (sx + fromIntegral gl)        (sy + fromIntegral gt)
                                 (sw - fromIntegral (gl + gr)) (sh - fromIntegral (gt + gb))

        -- just the tiled windows:
        -- now tile the windows on this workspace, modified by the gap
        rs <- runLayout l viewrect tiled `catchX` runLayout full viewrect tiled
        mapM_ (uncurry tileWindow) rs

        -- now the floating windows:
        -- move/resize the floating windows, if there are any
        forM_ flt $ \fw -> whenJust (M.lookup fw (W.floating ws)) $
          \(W.RationalRect rx ry rw rh) -> do
            tileWindow fw $ Rectangle
                (sx + floor (toRational sw*rx)) (sy + floor (toRational sh*ry))
                (floor (toRational sw*rw)) (floor (toRational sh*rh))

        let vs = flt ++ map fst rs
        io $ restackWindows d vs
        -- return the visible windows for this workspace:
        return vs

    setTopFocus
    logHook
    -- io performGC -- really helps, but seems to trigger GC bugs?

    -- hide every window that was potentially visible before, but is not
    -- given a position by a layout now.
    mapM_ hide (nub oldvisible \\ visible)

    clearEnterEvents

-- | setWMState.  set the WM_STATE property
setWMState :: Window -> Int -> X ()
setWMState w v = withDisplay $ \dpy -> do
    a <- atom_WM_STATE
    io $ changeProperty32 dpy w a a propModeReplace [fromIntegral v, fromIntegral none]

-- | hide. Hide a window by unmapping it, and setting Iconified.
hide :: Window -> X ()
hide w = whenX (gets (S.member w . mapped)) $ withDisplay $ \d -> do
    io $ do selectInput d w (clientMask .&. complement structureNotifyMask)
            unmapWindow d w
            selectInput d w clientMask
    setWMState w 3 --iconic
    -- this part is key: we increment the waitingUnmap counter to distinguish
    -- between client and xmonad initiated unmaps.
    modify (\s -> s { waitingUnmap = M.insertWith (+) w 1 (waitingUnmap s)
                    , mapped       = S.delete w (mapped s) })

-- | reveal. Show a window by mapping it and setting Normal
-- this is harmless if the window was already visible
reveal :: Window -> X ()
reveal w = withDisplay $ \d -> do
    setWMState w 1 --normal
    io $ mapWindow d w
    modify (\s -> s { mapped = S.insert w (mapped s) })

-- | The client events that xmonad is interested in
clientMask :: EventMask
clientMask = structureNotifyMask .|. enterWindowMask .|. propertyChangeMask

-- | Set some properties when we initially gain control of a window
setInitialProperties :: Window -> X ()
setInitialProperties w = withDisplay $ \d -> io $ do
    selectInput d w $ clientMask
    setWindowBorderWidth d w borderWidth

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
    -- give all windows at least 1x1 pixels
    let least x | x <= bw*2  = 1
                | otherwise  = x - bw*2
    io $ moveResizeWindow d w (rect_x r) (rect_y r)
                              (least $ rect_width r) (least $ rect_height r)
    reveal w

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
            io $ setWindowBorder dpy otherw nbc

    -- If we ungrab buttons on the root window, we lose our mouse bindings.
    whenX (not `liftM` isRoot w) $ setButtonGrab False w
    io $ do setInputFocus dpy w revertToPointerRoot 0
            -- raiseWindow dpy w
    io $ setWindowBorder dpy w fbc

-- ---------------------------------------------------------------------
-- Managing layout

-- | switchLayout.  Switch to another layout scheme.  Switches the
-- layout of the current workspace. By convention, a window set as
-- master in Tall mode remains as master in Wide mode. When switching
-- from full screen to a tiling mode, the currently focused window
-- becomes a master. When switching back , the focused window is
-- uppermost.
--
-- Note that the new layout's deconstructor will be called, so it should be
-- idempotent.
switchLayout :: X ()
switchLayout = do
    broadcastMessage UnDoLayout  -- calling refresh now would defeat the point of deconstruction
    n <- gets (W.tag . W.workspace . W.current . windowset)
    modify $ \s -> s { layouts = M.adjust switch n (layouts s) }
    refresh
 where switch (x, xs) = let xs' =  xs ++ [x] in (head xs', tail xs')

-- | Throw a message to the current Layout possibly modifying how we
-- layout the windows, then refresh.
--
sendMessage :: Message a => a -> X ()
sendMessage a = do n <- (W.tag . W.workspace . W.current) `fmap` gets windowset
                   Just (l,ls) <- M.lookup n `fmap` gets layouts
                   ml' <- modifyLayout l (SomeMessage a) `catchX` return (Just l)
                   whenJust ml' $ \l' -> do modify $ \s -> s { layouts = M.insert n (l',ls) (layouts s) }
                                            refresh

-- | Send a message to all visible layouts, without necessarily refreshing.
-- This is how we implement the hooks, such as UnDoLayout.
broadcastMessage :: Message a => a -> X ()
broadcastMessage a = do
    ol <- gets layouts
    nl <- T.forM ol $ \ (l,ls) -> maybe (l,ls) (flip (,) ls) `fmap`
          (modifyLayout l (SomeMessage a) `catchX` return (Just l))
    modify $ \s -> s { layouts = nl }

instance Message Event

--
-- Builtin layout algorithms:
--
--   fullscreen mode
--   tall mode
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
full :: Layout a
full = Layout { doLayout     = \sc (W.Stack f _ _) -> return [(f, sc)]
              , modifyLayout = const (return Nothing) } -- no changes

--
-- The tiling mode of xmonad, and its operations.
--
tall :: Int -> Rational -> Rational -> Layout a
tall nmaster delta frac =
    Layout { doLayout     = \r -> return . ap zip (tile frac r nmaster . length) . W.integrate
           , modifyLayout = \m -> return $ msum [fmap resize     (fromMessage m)
                                                ,fmap incmastern (fromMessage m)] }

    where resize Shrink = tall nmaster delta (max 0 $ frac-delta)
          resize Expand = tall nmaster delta (min 1 $ frac+delta)
          incmastern (IncMasterN d) = tall (max 0 (nmaster+d)) delta frac

-- | Mirror a rectangle
mirrorRect :: Rectangle -> Rectangle
mirrorRect (Rectangle rx ry rw rh) = (Rectangle ry rx rh rw)

-- | Mirror a layout, compute its 90 degree rotated form.
mirror :: Layout a -> Layout a
mirror (Layout { doLayout = dl, modifyLayout = ml }) =
    Layout { doLayout     = \sc w -> map (second mirrorRect) `fmap` dl (mirrorRect sc) w
           , modifyLayout = fmap (fmap mirror) . ml }

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

-- | Combinations of extra modifier masks we need to grab keys\/buttons for.
-- (numlock and capslock)
extraModifiers :: [KeyMask]
extraModifiers = [0, numlockMask, lockMask, numlockMask .|. lockMask ]

-- | Strip numlock\/capslock from a mask
cleanMask :: KeyMask -> KeyMask
cleanMask = (complement (numlockMask .|. lockMask) .&.)

-- | Get the Pixel value for a named color
initColor :: Display -> String -> IO Pixel
initColor dpy c = (color_pixel . fst) `liftM` allocNamedColor dpy colormap c
    where colormap = defaultColormap dpy (defaultScreen dpy)

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

-- ---------------------------------------------------------------------
-- Mouse handling

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
    sh <- io $ getWMNormalHints d w
    io $ warpPointer d none w 0 0 0 0 (fromIntegral (wa_width wa)) (fromIntegral (wa_height wa))
    mouseDrag $ \(_, _, _, ex, ey, _, _, _, _, _) ->
        resizeWindow d w `uncurry`
             applySizeHints sh ((fromIntegral (max 1 (ex - fromIntegral (wa_x wa)))),
                                (fromIntegral (max 1 (ey - fromIntegral (wa_y wa)))))
    float w

-- ---------------------------------------------------------------------
-- | Support for window size hints

type D = (Dimension, Dimension)

-- | Reduce the dimensions if needed to comply to the given SizeHints.
applySizeHints :: SizeHints -> D -> D
applySizeHints sh =
      maybe id applyMaxSizeHint                   (sh_max_size   sh)
    . maybe id (\(bw, bh) (w, h) -> (w+bw, h+bh)) (sh_base_size  sh)
    . maybe id applyResizeIncHint                 (sh_resize_inc sh)
    . maybe id applyAspectHint                    (sh_aspect     sh)
    . maybe id (\(bw,bh) (w,h)   -> (w-bw, h-bh)) (sh_base_size  sh)

-- | Reduce the dimensions so their aspect ratio falls between the two given aspect ratios.
applyAspectHint :: (D, D) -> D -> D
applyAspectHint ((minx, miny), (maxx, maxy)) x@(w,h)
    | or [minx < 1, miny < 1, maxx < 1, maxy < 1] = x
    | w * maxy > h * maxx                         = (h * maxx `div` maxy, h)
    | w * miny < h * minx                         = (w, w * miny `div` minx)
    | otherwise                                   = x

-- | Reduce the dimensions so they are a multiple of the size increments.
applyResizeIncHint :: D -> D -> D
applyResizeIncHint (iw,ih) x@(w,h) =
    if iw > 0 && ih > 0 then (w - w `mod` iw, h - h `mod` ih) else x

-- | Reduce the dimensions if they exceed the given maximum dimensions.
applyMaxSizeHint  :: D -> D -> D
applyMaxSizeHint (mw,mh) x@(w,h) =
    if mw > 0 && mh > 0 then (min w mw,min h mh) else x
