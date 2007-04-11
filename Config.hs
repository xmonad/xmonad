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
-----------------------------------------------------------------------------

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
-- 
--     mod-tab                 raise next window in stack
--     mod-j
--     mod-k
-- 
--     mod-h                   resize currently focused window
--     mod-l
-- 
--     mod-shift-c             kill client
--     mod-shift-q             exit window manager
--     mod-shift-ctrl-q        restart window manager
-- 
--     mod-return              cycle the current tiling order
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

import Data.Ratio
import Data.Bits
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib
import XMonad
import Operations

-- The number of workspaces:
workspaces :: Int
workspaces = 9

-- modMask lets you easily change which modkey you use. The default is mod1Mask
-- ("left alt").  You may also consider using mod3mask ("right alt"), which
-- does not conflict with emacs keybindings.
modMask :: KeyMask
modMask = mod1Mask

-- How much to change the horizontal/vertical split bar by defalut.
defaultDelta :: Rational
defaultDelta = 3%100

-- How much to change the size of a tiled window, by default.
sizeDelta :: Rational
sizeDelta = 3%100

-- The mask for the numlock key. You may need to change this on some systems.
numlockMask :: KeyMask
numlockMask = lockMask

-- What layout to start in, and what the default proportion for the
-- left pane should be in the tiled layout.  See LayoutDesc and
-- friends in XMonad.hs for options.
startingLayoutDesc :: LayoutDesc
startingLayoutDesc =
    LayoutDesc { layoutType   = Full
               , tileFraction = 1%2  }

-- The keys list.
keys :: M.Map (KeyMask, KeySym) (X ())
keys = M.fromList $
    [ ((modMask .|. shiftMask, xK_Return), spawn "xterm")
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe")
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun")
    , ((modMask,               xK_space ), switchLayout)

    , ((modMask,               xK_Tab   ), raise GT)
    , ((modMask,               xK_j     ), raise GT)
    , ((modMask,               xK_k     ), raise LT)

    , ((modMask,               xK_h     ), changeSplit (negate defaultDelta))
    , ((modMask,               xK_l     ), changeSplit defaultDelta)

    , ((modMask .|. shiftMask, xK_c     ), kill)

    , ((modMask .|. shiftMask, xK_q                     ), io $ exitWith ExitSuccess)
    , ((modMask .|. shiftMask .|. controlMask, xK_q     ), io restart)

    -- Cycle the current tiling order
    , ((modMask,               xK_Return), promote)

    ] ++
    -- Keybindings to get to each workspace:
    [((m .|. modMask, xK_0 + fromIntegral i), f (fromIntegral (pred i))) -- index from 0.
        | i <- [1 .. workspaces]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]

    -- Keybindings to each screen :
    -- mod-wer (underneath 123) switches to physical/Xinerama screens 1 2 and 3
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= f)
        | (key, sc) <- zip [xK_s, xK_d, xK_f] [0..]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]

