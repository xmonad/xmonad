module Config where
import Graphics.X11.Xlib.Types (Dimension)
import Graphics.X11.Xlib (KeyMask,Window)
import XMonad
borderWidth :: Dimension
logHook     :: X ()
numlockMask :: KeyMask
workspaces :: [WorkspaceId]
possibleLayouts :: [SomeLayout Window]
