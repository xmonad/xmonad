module Config where
import Graphics.X11.Xlib.Types (Dimension)
import Graphics.X11.Xlib (KeyMask,Window)
import Graphics.X11.Xlib.Extras (ClassHint)
import XMonad
borderWidth :: Dimension
logHook     :: X ()
numlockMask :: KeyMask
workspaces :: [WorkspaceId]
possibleLayouts :: [SomeLayout Window]
manageHook :: Window -> ClassHint -> X (WindowSet -> WindowSet)
