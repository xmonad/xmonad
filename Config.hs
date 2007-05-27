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
    [ ((modMask .|. shiftMask, xK_Return), spawn "xterm") -- @@ Launch an xterm
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && exec $exe") -- @@ Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- @@ Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- @@ Close the focused window

    , ((modMask,               xK_space ), switchLayout) -- @@ Rotate through the available layout algorithms

    , ((modMask,               xK_n     ), refresh) -- 'nudge': resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), focusDown) -- @@ Move focus to the next window
    , ((modMask,               xK_j     ), focusDown) -- @@ Move focus to the next window
    , ((modMask,               xK_k     ), focusUp  ) -- @@ Move focus to the previous window

    -- modifying the window order
    , ((modMask,               xK_Return), swapMaster) -- @@ Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_j     ), swapDown  ) -- @@ Swap the focused window with the next window
    , ((modMask .|. shiftMask, xK_k     ), swapUp    ) -- @@ Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((modMask,               xK_h     ), sendMessage Shrink) -- @@ Shrink the master area
    , ((modMask,               xK_l     ), sendMessage Expand) -- @@ Expand the master area

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- @@ Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- @@ Deincrement the number of windows in the master area

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q                     ), io (exitWith ExitSuccess)) -- @@ Quit xmonad
    , ((modMask .|. shiftMask .|. controlMask, xK_q     ), restart Nothing True) -- @@ Restart xmonad

    ] ++
    -- mod-[1..9] @@ Switch to workspace N
    -- mod-shift-[1..9] @@ Move client to workspace N
    [((m .|. modMask, k), f i)
        | (i, k) <- zip [0 .. fromIntegral workspaces - 1] [xK_1 ..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]

    -- mod-{w,e,r} @@ Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} @@ Move client to screen 1, 2, or 3
    ++
    [((m .|. modMask, key), screenWorkspace sc >>= f)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(view, 0), (shift, shiftMask)]]
