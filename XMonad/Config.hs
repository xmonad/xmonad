{-# OPTIONS -fno-warn-missing-signatures #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Config
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  dons@galois.com
-- Stability   :  stable
-- Portability :  portable
--
-- This module specifies the default configuration values for xmonad.
--
-- DO NOT MODIFY THIS FILE!  It won't work.  You may configure xmonad
-- by providing your own @~\/.xmonad\/xmonad.hs@ that overrides
-- specific fields in 'defaultConfig'.  For a starting point, you can
-- copy the @xmonad.hs@ found in the @man@ directory, or look at
-- examples on the xmonad wiki.
--
------------------------------------------------------------------------

module XMonad.Config (defaultConfig) where

--
-- Useful imports
--
import XMonad.Core as XMonad hiding
    (workspaces,manageHook,numlockMask,keys,logHook,startupHook,borderWidth,mouseBindings
    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse)
import qualified XMonad.Core as XMonad
    (workspaces,manageHook,numlockMask,keys,logHook,startupHook,borderWidth,mouseBindings
    ,layoutHook,modMask,terminal,normalBorderColor,focusedBorderColor,focusFollowsMouse)

import XMonad.Layout
import XMonad.Operations
import XMonad.ManageHook
import qualified XMonad.StackSet as W
import Data.Bits ((.|.))
import qualified Data.Map as M
import System.Exit
import Graphics.X11.Xlib

-- | The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
workspaces :: [WorkspaceId]
workspaces = map show [1 .. 9 :: Int]

-- | modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
defaultModMask :: KeyMask
defaultModMask = mod1Mask

-- | The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
numlockMask :: KeyMask
numlockMask = mod2Mask

-- | Width of the window border in pixels.
--
borderWidth :: Dimension
borderWidth = 1

-- | Border colors for unfocused and focused windows, respectively.
--
normalBorderColor, focusedBorderColor :: String
normalBorderColor  = "gray" -- "#dddddd"
focusedBorderColor = "red"  -- "#ff0000" don't use hex, not <24 bit safe

------------------------------------------------------------------------
-- Window rules

-- | Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
--  xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
manageHook :: ManageHook
manageHook = composeAll
                [ className =? "MPlayer"        --> doFloat
                , className =? "Gimp"           --> doFloat ]

------------------------------------------------------------------------
-- Logging

-- | Perform an arbitrary action on each internal state change or X event.
-- Examples include:
--      * do nothing
--      * log the state to stdout
--
-- See the 'DynamicLog' extension for examples.
--
logHook :: X ()
logHook = return ()

-- | Perform an arbitrary action at xmonad startup.
startupHook :: X ()
startupHook = return ()

------------------------------------------------------------------------
-- Extensible layouts
--
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--

-- | The available layouts.  Note that each layout is separated by |||, which
-- denotes layout choice.
layout = tiled ||| Mirror tiled ||| Full
  where
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

     -- Default proportion of screen occupied by master pane
     ratio   = 1/2

     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

------------------------------------------------------------------------
-- Key bindings:

-- | The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
terminal :: String
terminal = "xterm"

-- | Whether focus follows the mouse pointer.
focusFollowsMouse :: Bool
focusFollowsMouse = True

-- | The xmonad key bindings. Add, modify or remove key bindings here.
--
-- (The comment formatting character is used when generating the manpage)
--
keys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((modMask,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"") -- %! Launch dmenu
    , ((modMask .|. shiftMask, xK_p     ), spawn "gmrun") -- %! Launch gmrun
    , ((modMask .|. shiftMask, xK_c     ), kill) -- %! Close the focused window

    , ((modMask,               xK_space ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((modMask,               xK_n     ), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((modMask,               xK_Tab   ), windows W.focusDown) -- %! Move focus to the next window
    , ((modMask .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- %! Move focus to the previous window
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

    -- floating layer support
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- toggle the status bar gap
    --, ((modMask              , xK_b     ), modifyGap (\i n -> let x = (XMonad.defaultGaps conf ++ repeat (0,0,0,0)) !! i in if n == x then (0,0,0,0) else x)) -- %! Toggle the status bar gap

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io (exitWith ExitSuccess)) -- %! Quit xmonad
    , ((modMask              , xK_q     ), restart "xmonad" True) -- %! Restart xmonad
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- | Mouse bindings: default actions bound to mouse events
--
mouseBindings :: XConfig Layout -> M.Map (KeyMask, Button) (Window -> X ())
mouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1 %! Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w
                                          >> windows W.swapMaster))
    -- mod-button2 %! Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3 %! Set the window to floating mode and resize by dragging
    , ((modMask, button3), (\w -> focus w >> mouseResizeWindow w
                                          >> windows W.swapMaster))
    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

-- | And, finally, the default set of configuration values itself
defaultConfig = XConfig
    { XMonad.borderWidth        = borderWidth
    , XMonad.workspaces         = workspaces
    , XMonad.layoutHook         = layout
    , XMonad.terminal           = terminal
    , XMonad.normalBorderColor  = normalBorderColor
    , XMonad.focusedBorderColor = focusedBorderColor
    , XMonad.numlockMask        = numlockMask
    , XMonad.modMask            = defaultModMask
    , XMonad.keys               = keys
    , XMonad.logHook            = logHook
    , XMonad.startupHook        = startupHook
    , XMonad.mouseBindings      = mouseBindings
    , XMonad.manageHook         = manageHook
    , XMonad.focusFollowsMouse  = focusFollowsMouse }
