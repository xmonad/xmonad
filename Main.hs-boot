module Main where
import Graphics.X11.Xlib (KeyMask,Window)
import XMonad
numlockMask :: KeyMask
manageHook :: Window -> String -> String -> String -> X (WindowSet -> WindowSet)
