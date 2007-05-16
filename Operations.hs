{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Operations.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable
--
-----------------------------------------------------------------------------

module Operations where

import Data.List
import Data.Maybe
import Data.Bits
import qualified Data.Map as M

import Control.Monad.State
import Control.Monad.Reader
import Control.Arrow (second)

import System.Posix.Process
import System.Environment
import System.Directory

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import XMonad
import {-# SOURCE #-} Config

import qualified StackSet as W


-- ---------------------------------------------------------------------
-- Managing windows

-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
refresh :: X ()
refresh = do
    XState { workspace = ws, layouts = fls } <- get
    XConf  { xineScreens = xinesc, display = d } <- ask -- neat, eh?

    flip mapM_ (M.assocs (W.screen2ws ws)) $ \(scn, n) -> do
        let sc        = genericIndex xinesc scn -- temporary coercion!
            (Just l)  = fmap fst $ M.lookup n fls
        whenJust (W.index n ws) $ \winds ->
            do wrects <- doLayout l sc winds :: X [(Window,Rectangle)]
               mapM_ (\(w, rect) -> io $ moveWindowInside d w rect) wrects
        whenJust (W.peekStack n ws) (io . raiseWindow d)
    whenJust (W.peek ws) setFocus
    clearEnterEvents

-- | clearEnterEvents.  Remove all window entry events from the event queue.
clearEnterEvents :: X ()
clearEnterEvents = do
    d <- asks display
    io $ sync d False
    io $ allocaXEvent $ \p -> fix $ \again -> do
        more <- checkMaskEvent d enterWindowMask p
        when more again -- beautiful

------------------------------------------------------------------------

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

------------------------------------------------------------------------
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

tall nmaster delta frac = Layout { doLayout = \r w -> return $ zip w $ tile frac r nmaster (length w)
                                 , modifyLayout = \m -> fmap resize (fromMessage m) `mplus` fmap incmastern (fromMessage m) }

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
tile _ r nmaster n | n <= nmaster = splitVertically n r
tile f r nmaster n = splitVertically nmaster r1 ++ splitVertically (n-nmaster) r2
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
        let n          = W.current . workspace $ s
            (Just fl)  = M.lookup n $ layouts s
        in s { layouts = M.insert n (f fl) (layouts s) }
    refresh

-- | windows. Modify the current window list with a pure function, and refresh
windows :: (WindowSet -> WindowSet) -> X ()
windows f = do
    modify $ \s -> s { workspace = f (workspace s) }
    refresh
    -- gets workspace >>= trace . show -- log state changes to stderr

-- | hide. Hide a window by moving it offscreen.
hide :: Window -> X ()
hide w = withDisplay $ \d -> do
    (sw,sh) <- asks dimensions
    io $ moveWindow d w (2*fromIntegral sw) (2*fromIntegral sh)

-- ---------------------------------------------------------------------
-- Window operations

-- | setButtonGrab. Tell whether or not to intercept clicks on a given window
buttonsToGrab :: [Button]
buttonsToGrab = [button1, button2, button3]

setButtonGrab :: Bool -> Window -> X ()
setButtonGrab True  w = withDisplay $ \d -> io $
    flip mapM_ buttonsToGrab $ \b ->
        grabButton d b anyModifier w False
                   (buttonPressMask .|. buttonReleaseMask)
                   grabModeAsync grabModeSync none none

setButtonGrab False w = withDisplay $ \d -> io $
    flip mapM_ buttonsToGrab $ \b ->
        ungrabButton d b anyModifier w

-- | moveWindowInside. Moves and resizes w such that it fits inside the given
-- rectangle, including its border.
moveWindowInside :: Display -> Window -> Rectangle -> IO ()
moveWindowInside d w r = do
    bw <- (fromIntegral . wa_border_width) `liftM` getWindowAttributes d w
    moveResizeWindow d w (rect_x r) (rect_y r)
                         (rect_width  r - bw*2)
                         (rect_height r - bw*2)

-- | manage. Add a new window to be managed in the current workspace. Bring it into focus.
-- If the window is already under management, it is just raised.
--
manage :: Window -> X ()
manage w = do
    withDisplay $ \d -> io $ do
        selectInput d w $ structureNotifyMask .|. enterWindowMask .|. propertyChangeMask
        mapWindow d w
        setWindowBorderWidth d w borderWidth
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

safeFocus :: Window -> X ()
safeFocus w = do ws <- gets workspace
                 if W.member w ws
                    then setFocus w
                    else do b <- isRoot w
                            when b setTopFocus

-- | Explicitly set the keyboard focus to the given window
setFocus :: Window -> X ()
setFocus w = do
    ws <- gets workspace
    XConf { display = dpy , normalBorder = nbc, focusedBorder = fbc } <- ask

    -- clear mouse button grab and border on other windows
    flip mapM_ (W.visibleWorkspaces ws) $ \n -> do
        flip mapM_ (fromMaybe [] $ W.index n ws) $ \otherw -> do
            setButtonGrab True otherw
            io $ setWindowBorder dpy otherw (color_pixel nbc)

    withDisplay $ \d -> io $ setInputFocus d w revertToPointerRoot 0
    setButtonGrab False w
    io $ setWindowBorder dpy w (color_pixel fbc)

    -- This does not use 'windows' intentionally.  'windows' calls refresh,
    -- which means infinite loops.
    modify $ \s -> s { workspace = W.raiseFocus w (workspace s) }

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = do
    ws <- gets workspace
    case W.peek ws of
        Just new -> setFocus new
        Nothing  -> asks theRoot >>= setFocus

-- | raise. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
raise :: Ordering -> X ()
raise = windows . W.rotate

-- | promote. Move the currently focused window into the master frame
promote :: X ()
promote = windows W.promote

-- | Kill the currently focused client
kill :: X ()
kill = withDisplay $ \d -> do
    ws <- gets workspace
    whenJust (W.peek ws) $ \w -> do
        protocols <- io $ getWMProtocols d w
        XConf {wmdelete = wmdelt, wmprotocols = wmprot} <- ask
        if wmdelt `elem` protocols
            then io $ allocaXEvent $ \ev -> do
                    setEventType ev clientMessage
                    setClientMessageEvent ev w wmprot 32 wmdelt 0
                    sendEvent d w False noEventMask ev
            else io (killClient d w) >> return ()

-- | tag. Move a window to a new workspace, 0 indexed.
tag :: WorkspaceId -> X ()
tag n = do
    ws <- gets workspace
    let m = W.current ws -- :: WorkspaceId
    when (n /= m) $
        whenJust (W.peek ws) $ \w -> do
            hide w
            windows $ W.shift n

-- | view. Change the current workspace to workspace at offset n (0 indexed).
view :: WorkspaceId -> X ()
view n = do
    ws <- gets workspace
    let m = W.current ws
    windows $ W.view n
    ws' <- gets workspace
    -- If the old workspace isn't visible anymore, we have to hide the windows
    -- in case we're switching to an empty workspace.
    when (m `notElem` W.visibleWorkspaces ws') $ maybe (return ()) (mapM_ hide) $ W.index m ws
    clearEnterEvents
    setTopFocus

-- | 'screenWorkspace sc' returns the workspace number viewed by 'sc'.
screenWorkspace :: ScreenId -> X WorkspaceId
screenWorkspace sc = fmap (fromMaybe 0 . W.workspace sc) (gets workspace)

-- | True if window is under management by us
isClient :: Window -> X Bool
isClient w = liftM (W.member w) (gets workspace)

-- | Restart xmonad by exec()'ing self. This doesn't save state and xmonad has
-- to be in PATH for this to work.
restart :: IO ()
restart = do
    prog      <- getProgName
    prog_path <- findExecutable prog
    case prog_path of
        Nothing -> return ()    -- silently fail
        Just p  -> do args <- getArgs
                      executeFile p True args Nothing
