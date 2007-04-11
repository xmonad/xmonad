module Operations where

import Data.List
import Data.Maybe
import Data.Bits
import qualified Data.Map as M

import Control.Monad.State
import Control.Arrow

import System.Posix.Process
import System.Environment

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras

import XMonad

import qualified StackSet as W


-- ---------------------------------------------------------------------
-- Managing windows

-- | refresh. Refresh the currently focused window. Resizes to full
-- screen and raises the window.
refresh :: X ()
refresh = do
    XState {workspace = ws, xineScreens = xinesc
           ,display   = d ,layoutDescs = fls  ,defaultLayoutDesc = dfltfl } <- get

    flip mapM_ (M.assocs (W.screen2ws ws)) $ \(scn, n) -> do
        let sc =  genericIndex xinesc scn -- temporary coercion!
            fl = M.findWithDefault dfltfl n fls
        mapM_ (\(w, rect) -> io $ moveWindowInside d w rect) $
            case layoutType fl of
                Full -> fmap (flip (,) sc) $ maybeToList $ W.peekStack n ws
                Tall -> tile  (tileFraction fl) sc $ W.index n ws
                Wide -> vtile (tileFraction fl) sc $ W.index n ws
        whenJust (W.peekStack n ws) (io . raiseWindow d)
    whenJust (W.peek ws) setFocus

-- | tile.  Compute the positions for windows in horizontal layout
-- mode.
--
tile :: Rational -> Rectangle -> [Window] -> [(Window, Rectangle)]
tile _ _ []    = []
tile _ d [w]   = [(w, d)]
tile r (Rectangle sx sy sw sh) (w:s)
 = (w, Rectangle sx sy (fromIntegral lw) sh) : zipWith f [sy, sy + rh ..] s
 where
    lw = floor $ fromIntegral sw * r
    rw = sw - fromIntegral lw
    rh = fromIntegral sh `div` fromIntegral (length s)
    f i a = (a, Rectangle (sx + lw) i rw (fromIntegral rh))

-- | vtile. Tile vertically.
vtile :: Rational -> Rectangle -> [Window] -> [(Window, Rectangle)]
vtile r rect = map (second flipRect) . tile r (flipRect rect)

-- | Flip rectangles around
flipRect :: Rectangle -> Rectangle
flipRect (Rectangle rx ry rw rh) = (Rectangle ry rx rh rw)

-- | switchLayout.  Switch to another layout scheme.  Switches the
-- current workspace. By convention, a window set as master in Tall mode
-- remains as master in Wide mode. When switching from full screen to a
-- tiling mode, the currently focused window becomes a master. When
-- switching back , the focused window is uppermost.
--
-- Note a current `feature' is that 'promote' cycles clockwise in Tall
-- mode, and counter clockwise in wide mode. This is a feature.
--
switchLayout :: X ()
switchLayout = layout $ \fl -> fl { layoutType = rotateLayout (layoutType fl) }

-- | changeSplit.  Changes the window split.
changeSplit :: Rational -> X ()
changeSplit delta = layout $ \fl ->
    fl { tileFraction = min 1 (max 0 (tileFraction fl + delta)) }

-- | layout. Modify the current workspace's layout with a pure
-- function and refresh.
layout :: (LayoutDesc -> LayoutDesc) -> X ()
layout f = do
    modify $ \s ->
        let fls = layoutDescs s
            n   = W.current . workspace $ s
            fl  = M.findWithDefault (defaultLayoutDesc s) n fls
        in s { layoutDescs = M.insert n (f fl) fls }
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
    bw <- (fromIntegral . waBorderWidth) `liftM` getWindowAttributes d w
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

    -- clear mouse button grab and border on other windows
    flip mapM_ (W.visibleWorkspaces ws) $ \n -> do
        flip mapM_ (W.index n ws) $ \otherw -> do
            setButtonGrab True otherw
            setBorder otherw 0xdddddd

    withDisplay $ \d -> io $ setInputFocus d w revertToPointerRoot 0
    setButtonGrab False w
    setBorder w 0xff0000    -- make this configurable

    -- This does not use 'windows' intentionally.  'windows' calls refresh,
    -- which means infinite loops.
    modify $ \s -> s { workspace = W.raiseFocus w (workspace s) }

-- | Set the focus to the window on top of the stack, or root
setTopFocus :: X ()
setTopFocus = do
    ws <- gets workspace
    case W.peek ws of
        Just new -> setFocus new
        Nothing  -> gets theRoot >>= setFocus

-- | Set the border color for a particular window.
setBorder :: Window -> Pixel -> X ()
setBorder w p = withDisplay $ \d -> io $ setWindowBorder d w p

-- | raise. focus to window at offset 'n' in list.
-- The currently focused window is always the head of the list
raise :: Ordering -> X ()
raise = windows . W.rotate

-- | promote. Cycle the current tiling order clockwise.
promote :: X ()
promote = windows W.promote

-- | Kill the currently focused client
kill :: X ()
kill = withDisplay $ \d -> do
    ws <- gets workspace
    whenJust (W.peek ws) $ \w -> do
        protocols <- io $ getWMProtocols d w
        XState {wmdelete = wmdelt, wmprotocols = wmprot} <- get
        if wmdelt `elem` protocols
            then io $ allocaXEvent $ \ev -> do
                    setEventType ev clientMessage
                    setClientMessageEvent ev w wmprot 32 wmdelt 0
                    sendEvent d w False noEventMask ev
            else io (killClient d w) >> return ()

-- | tag. Move a window to a new workspace, 0 indexed.
tag :: W.WorkspaceId -> X ()
tag n = do
    ws <- gets workspace
    let m = W.current ws -- :: WorkspaceId
    when (n /= m) $
        whenJust (W.peek ws) $ \w -> do
            hide w
            windows $ W.shift n

-- | view. Change the current workspace to workspce at offset n (0 indexed).
view :: W.WorkspaceId -> X ()
view n = do
    ws <- gets workspace
    let m = W.current ws
    windows $ W.view n
    ws' <- gets workspace
    -- If the old workspace isn't visible anymore, we have to hide the windows
    -- in case we're switching to an empty workspace.
    when (m `notElem` (W.visibleWorkspaces ws')) (mapM_ hide (W.index m ws))
    setTopFocus

-- | 'screenWorkspace sc' returns the workspace number viewed by 'sc'.
screenWorkspace :: W.ScreenId -> X W.WorkspaceId
screenWorkspace sc = fmap (fromMaybe 0 . W.workspace sc) (gets workspace)

-- | True if window is under management by us
isClient :: Window -> X Bool
isClient w = liftM (W.member w) (gets workspace)

-- | Restart xmonad by exec()'ing self. This doesn't save state and xmonad has
-- to be in PATH for this to work.
restart :: IO ()
restart = do
    prog <- getProgName
    args <- getArgs
    executeFile prog True args Nothing
