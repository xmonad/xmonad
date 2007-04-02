module Config where

{-
xmonad places each window into a "workspace." Each workspace can have any
number of windows, which you can cycle though with mod-j and mod-k. Windows are
either displayed full screen, or tiled. You can toggle the layout mode with
mod-space.

You can switch to workspace N with mod-N. For example, to switch to workspace
5, you would press mod-5. Similarly, you can move the current window to another
workspace with mod-shift-N.

When running with multiple monitors (Xinerama), each screen has exactly 1
workspace visible. When xmonad starts, workspace 1 is on screen 1, workspace 2
is on screen 2, etc. If you switch to a workspace which is currently visible on
another screen, xmonad simply switches focus to that screen. If you switch to a
workspace which is *not* visible, xmonad replaces the workspace on the
*current* screen with the workspace you selected.

For example, if you have the following configuration:

Screen 1: Workspace 2
Screen 2: Workspace 5 (current workspace)

and you wanted to view workspace 7 on screen 1, you would press:

mod-2 (to select workspace 2, and make screen 1 the current screen)
mod-7 (to select workspace 7)

Since switching to the workspace currently visible on a given screen is such a
common operation, shortcuts are provided: mod-{w,e,r} switch to the workspace
currently visible on screens 1, 2, and 3 respectively. Likewise,
shift-mod-{w,e,r} moves the current window to the workspace on that screen.
Using these keys, the above example would become mod-w mod-7.
-}

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

    , ((modMask,               xK_h     ), changeHorz (negate defaultDelta))
    , ((modMask,               xK_l     ), changeHorz defaultDelta)
    -- Not implemented yet:
    -- , ((modMask .|. shiftMask, xK_j     ), changeVert defaultDelta)
    -- , ((modMask .|. shiftMask, xK_k     ), changeVert (negate defaultDelta))

    , ((modMask .|. shiftMask, xK_c     ), kill)

    , ((modMask .|. shiftMask, xK_q                     ), io $ exitWith ExitSuccess)
    , ((modMask .|. shiftMask .|. controlMask, xK_q     ), io restart)

    -- more focused window into master position in tiling mode.
    , ((modMask,               xK_Return), promote)

    ] ++
    -- Keybindings to get to each workspace:
    [((m .|. modMask, xK_0 + fromIntegral i), f i)
        | i <- [1 .. workspaces]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]

    -- Keybindings to each screen :
    -- mod-wer (underneath 123) switches to physical/Xinerama screens 1 2 and 3
    ++
    [((m .|. modMask, key), screenWS sc >>= f)
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [1..]
        , (f, m) <- [(view, 0), (tag, shiftMask)]]

