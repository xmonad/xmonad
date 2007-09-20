-----------------------------------------------------------------------------
-- |
-- Module      :  Config.hs
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
-- 
-- Maintainer  :  dons@cse.unsw.edu.au
-- Stability   :  stable
-- Portability :  portable
-- 
--
-- This module specifies configurable defaults for xmonad. If you change
-- values here, be sure to recompile and restart (mod-q) xmonad,
-- for the changes to take effect.
-- 
------------------------------------------------------------------------

module Config where

-- 
-- Useful imports
--
import XMonad
import Operations
import qualified StackSet as W
import Data.Ratio
import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib

-- Extension-provided imports

--
-- The number of workspaces (virtual screens, or window groups)
--
workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

-- |
-- modMask lets you specify which modkey you want to use. The default is
-- mod1Mask ("left alt").  You may also consider using mod3Mask ("right
-- alt"), which does not conflict with emacs keybindings. The "windows
-- key" is usually mod4Mask.
--
modMask :: KeyMask
modMask = mod1Mask

-- |
-- Default offset of drawable screen boundaries from each physical screen.
-- Anything non-zero here will leave a gap of that many pixels on the
-- given edge, on the that screen. A useful gap at top of screen for a
-- menu bar (e.g. 15)
-- 
-- An example, to set a top gap on monitor 1, and a gap on the bottom of
-- monitor 2, you'd use a list of geometries like so:
--
-- > defaultGaps = [(18,0,0,0),(0,18,0,0)] -- 2 gaps on 2 monitors
--
-- Fields are: top, bottom, left, right.
--
defaultGaps :: [(Int,Int,Int,Int)]
defaultGaps = [(0,0,0,0)] -- 15 for default dzen

-- |
-- numlock handling:
--
-- The mask for the numlock key. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- $ xmodmap | grep Num
-- mod2        Num_Lock (0x4d)
--
numlockMask :: KeyMask
numlockMask = mod2Mask

-- |
-- Border colors for unfocused and focused windows, respectively.
--
normalBorderColor, focusedBorderColor :: String
normalBorderColor  = "#dddddd"
focusedBorderColor = "#ff0000"

-- |
-- Width of the window border in pixels
--
borderWidth :: Dimension
borderWidth = 1

-- |
-- The default set of tiling algorithms
--
defaultLayouts :: [SomeLayout Window]
defaultLayouts = [ SomeLayout tiled
                 , SomeLayout $ Mirror tiled
                 , SomeLayout Full

                 -- Extension-provided layouts
                 ]
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1%2

     -- Percent of screen to increment by when resizing panes
     delta   = 3%100

-- |
-- Perform an arbitrary action on each state change.
-- Examples include:
--      * do nothing
--      * log the state to stdout
--
logHook :: X ()
logHook = return ()

-- |
-- The key bindings list.
-- 
-- The unusual comment format is used to generate the documentation
-- automatically.
--
keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn "xterm") -- %! Launch an xterm
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), switchLayout) -- %! Rotate through the available layout algorithms

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_j     ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask,               xK_k     ), windows W.focusUp  ) -- %! Move focus to the previous window
    , ((modMask,               xK_m     ), windows W.focusMaster  ) -- %! Move focus to the master window


    -- modifying the window order
    , ((modMask,               xK_Return), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- %! Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- %! Expand the master area

    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    , ((modMask              , xK_b     ), modifyGap (\i n -> let x = (defaultGaps ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart Nothing True) -- %! Restart xmonad

    -- Extension-provided key bindings
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip workspaces [xK_1 ..]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    -- Extension-provided key bindings lists

-- |
-- default actions bound to mouse events
--
mouseBindings :: M.Map (KeyMask, Button) (Window -> X ())
mouseBindings = M.fromList $
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w))
    -- Extension-provided mouse bindings
    ]

-- Extension-provided definitions
