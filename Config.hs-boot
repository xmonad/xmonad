module Config where
import Graphics.X11.Xlib.Types (Dimension)
import Graphics.X11.Xlib (KeyMask,Window)
import XMonad
borderWidth :: Dimension
numlockMask :: KeyMask
workspaces :: [WorkspaceId]
logHook     :: X ()
manageHook :: Window -> String -> String -> String -> X (WindowSet -> WindowSet)
serialisedLayouts :: [Layout Window]
