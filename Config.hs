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
------------------------------------------------------------------------
--
-- This module specifies configurable defaults for xmonad. If you change
-- values here, be sure to recompile and restart (mod-shift-ctrl-q) xmonad,
-- for the changes to take effect.
--

module Config where

--
-- xmonad bindings follow mostly the dwm/wmii conventions:
-- 
--     key combination         action
--     
--     mod-shift-return        new xterm
--     mod-p                   launch dmenu
--     mod-shift-p             launch gmrun
-- 
--     mod-space               switch tiling mode
--     mod-n                   nudge current window into fullscreen mode
-- 
--     mod-tab                 shift focus to next window in stack
--     mod-j                   shift focus to next window in stack
--     mod-k                   shift focus previous window in stack 
-- 
--     mod-h                   decrease the size of the master area
--     mod-l                   increase the size of the master area
-- 
--     mod-shift-c             kill client
--     mod-shift-q             exit window manager
--     mod-shift-ctrl-q        restart window manager ('xmonad' must be in $PATH)
-- 
--     mod-return              swap focused window with master window
-- 
--     mod-1..9                switch to workspace N
--     mod-shift-1..9          move client to workspace N
-- 
--     mod-w,e,r               switch to physical/Xinerama screen 1, 2 or 3.
-- 
-- xmonad places each window into a "workspace." Each workspace can have
-- any number of windows, which you can cycle though with mod-j and mod-k.
-- Windows are either displayed full screen, tiled horizontally, or tiled
-- vertically. You can toggle the layout mode with mod-space, which will
-- cycle through the available modes.
-- 
-- You can switch to workspace N with mod-N. For example, to switch to
-- workspace 5, you would press mod-5. Similarly, you can move the current
-- window to another workspace with mod-shift-N.
-- 
-- When running with multiple monitors (Xinerama), each screen has exactly
-- 1 workspace visible. When xmonad starts, workspace 1 is on screen 1,
-- workspace 2 is on screen 2, etc. If you switch to a workspace which is
-- currently visible on another screen, xmonad simply switches focus to
-- that screen. If you switch to a workspace which is *not* visible, xmonad
-- replaces the workspace on the *current* screen with the workspace you
-- selected.
-- 
-- For example, if you have the following configuration:
-- 
-- Screen 1: Workspace 2
-- Screen 2: Workspace 5 (current workspace)
-- 
-- and you wanted to view workspace 7 on screen 1, you would press:
-- 
-- mod-2 (to select workspace 2, and make screen 1 the current screen)
-- mod-7 (to select workspace 7)
-- 
-- Since switching to the workspace currently visible on a given screen is
-- such a common operation, shortcuts are provided: mod-{w,e,r} switch to
-- the workspace currently visible on screens 1, 2, and 3 respectively.
-- Likewise, shift-mod-{w,e,r} moves the current window to the workspace on
-- that screen.  Using these keys, the above example would become mod-w
-- mod-7.
-- 

-- 
-- Useful imports
--
import XMonad
import Operations
import Data.Ratio
import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib

-- The number of workspaces (virtual screens)
workspaces :: Int
workspaces = 9

-- modMask lets you specify which modkey you want to use. The default is mod1Mask
-- ("left alt").  You may also consider using mod3Mask ("right alt"), which
-- does not conflict with emacs keybindings. The "windows key" is usually
-- mod4Mask.
--
modMask :: KeyMask
modMask = mod1Mask

-- When resizing a window, this ratio specifies by what percent to
-- resize in a single step
defaultDelta :: Rational
defaultDelta = 3%100

-- The default number of windows in the master area
defaultWindowsInMaster :: Int
defaultWindowsInMaster = 1

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

-- Border colors for unfocused and focused windows, respectively.
normalBorderColor, focusedBorderColor :: String
normalBorderColor  = "#dddddd"
focusedBorderColor = "#ff0000"

-- Width of the window border in pixels
borderWidth :: Dimension
borderWidth = 1

-- The default set of Layouts:
defaultLayouts :: [Layout]
defaultLayouts = [ full
                 , tall defaultWindowsInMaster defaultDelta (1%2)
                 , wide defaultWindowsInMaster defaultDelta (1%2) ]

--
-- The key bindings list.
--
keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn "xterm")
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- rotate through the available layout algorithms
    , ((modMask,               xK_space ), switchLayout)

    -- 'nudge': resize viewed windows to the correct size.
    , ((modMask,               xK_n     ), refresh)

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), focusDown)
    , ((modMask,               xK_j     ), focusDown)
    , ((modMask,               xK_k     ), focusUp)

    -- modifying the window order
    , ((modMask,               xK_Return), swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), swapDown)
    , ((modMask .|. shiftMask, xK_k     ), swapUp)

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q                     ), io $ exitWith ExitSuccess)
    , ((modMask .|. shiftMask .|. controlMask, xK_q     ), restart Nothing True)

    ] ++
    -- Keybindings to get to each workspace:
    [((m .|. modMask, k), f i)
        | (i, k) <- zip [0 .. fromIntegral workspaces - 1] [xK_1 ..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]

    -- Keybindings to each screen :
    -- mod-wer (underneath 123) switches to physical/Xinerama screens 1 2 and 3
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= f)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]

