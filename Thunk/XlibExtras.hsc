module Thunk.XlibExtras where

import Graphics.X11.Xlib
import Graphics.X11.Xlib.Types
import Foreign
import Foreign.C.Types
import Control.Monad (ap)

#include "XlibExtras.h"

data Event 
    = AnyEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , window                :: Window
        }
    | ConfigureRequestEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , parent                :: Window
        , window                :: Window
        , x                     :: Int
        , y                     :: Int
        , width                 :: Int
        , height                :: Int
        , border_width          :: Int
        , above                 :: Window
        , detail                :: Int
        , value_mask            :: CULong
        }
    | MapRequestEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , parent                :: Window
        , window                :: Window
        }
    | KeyEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , window                :: Window
        , root                  :: Window
        , subwindow             :: Window
        , time                  :: Time
        , x                     :: Int
        , y                     :: Int
        , x_root                :: Int
        , y_root                :: Int
        , state                 :: KeyMask
        , keycode               :: KeyCode
        , same_screen           :: Bool
        }
    | DestroyWindowEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , event                 :: Window
        , window                :: Window
        }
    | UnmapEvent
        { event_type            :: EventType
        , serial                :: CULong
        , send_event            :: Bool
        , event_display         :: Display
        , event                 :: Window
        , window                :: Window
        , fromConfigure         :: Bool
        }
    deriving Show

getEvent :: XEventPtr -> IO Event
getEvent p = do
    -- All events share this layout and naming convention, there is also a
    -- common Window field, but the names for this field vary.
    type_ <- #{peek XAnyEvent, type} p
    serial_ <- #{peek XAnyEvent, serial} p
    send_event_ <- #{peek XAnyEvent, send_event} p
    display_ <- fmap Display (#{peek XAnyEvent, display} p)
    case () of

        -------------------------
        -- ConfigureRequestEvent:
        -------------------------
        _ | type_ == configureRequest -> do
            parent_       <- #{peek XConfigureRequestEvent, parent      } p
            window_       <- #{peek XConfigureRequestEvent, window      } p
            x_            <- #{peek XConfigureRequestEvent, x           } p
            y_            <- #{peek XConfigureRequestEvent, y           } p
            width_        <- #{peek XConfigureRequestEvent, width       } p
            height_       <- #{peek XConfigureRequestEvent, height      } p
            border_width_ <- #{peek XConfigureRequestEvent, border_width} p
            above_        <- #{peek XConfigureRequestEvent, above       } p
            detail_       <- #{peek XConfigureRequestEvent, detail      } p
            value_mask_   <- #{peek XConfigureRequestEvent, value_mask  } p
            return $ ConfigureRequestEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , parent        = parent_
                        , window        = window_
                        , x             = x_
                        , y             = y_
                        , width         = width_
                        , height        = height_
                        , border_width  = border_width_
                        , above         = above_
                        , detail        = detail_
                        , value_mask    = value_mask_
                        }

          -------------------
          -- MapRequestEvent:
          -------------------
          | type_ == mapRequest -> do
            parent_ <- #{peek XMapRequestEvent, parent} p
            window_ <- #{peek XMapRequestEvent, window} p
            return $ MapRequestEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , parent        = parent_
                        , window        = window_
                        }

          ------------
          -- KeyEvent:
          ------------
          | type_ == keyPress || type_ == keyRelease -> do
            window_      <- #{peek XKeyEvent, window     } p
            root_        <- #{peek XKeyEvent, root       } p
            subwindow_   <- #{peek XKeyEvent, subwindow  } p
            time_        <- #{peek XKeyEvent, time       } p
            x_           <- #{peek XKeyEvent, x          } p
            y_           <- #{peek XKeyEvent, y          } p
            x_root_      <- #{peek XKeyEvent, x_root     } p
            y_root_      <- #{peek XKeyEvent, y_root     } p
            state_       <- #{peek XKeyEvent, state      } p
            keycode_     <- #{peek XKeyEvent, keycode    } p
            same_screen_ <- #{peek XKeyEvent, same_screen} p
            return $ KeyEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , window        = window_
                        , root          = root_
                        , subwindow     = subwindow_
                        , time          = time_
                        , x             = x_
                        , y             = y_
                        , x_root        = x_root_
                        , y_root        = y_root_
                        , state         = state_
                        , keycode       = keycode_
                        , same_screen   = same_screen_
                        }

          ----------------------
          -- DestroyWindowEvent:
          ----------------------
          | type_ == destroyNotify -> do
            event_  <- #{peek XDestroyWindowEvent, event } p
            window_ <- #{peek XDestroyWindowEvent, window} p
            return $ DestroyWindowEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , event         = event_
                        , window        = window_
                        }


          --------------------
          -- UnmapNotifyEvent:
          --------------------
          | type_ == unmapNotify -> do
            event_         <- #{peek XUnmapEvent, event         } p
            window_        <- #{peek XUnmapEvent, window        } p
            fromConfigure_ <- #{peek XUnmapEvent, from_configure} p
            return $ UnmapEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , event         = event_
                        , window        = window_
                        , fromConfigure = fromConfigure_
                        }

          -- We don't handle this event specifically, so return the generic
          -- AnyEvent.
          | otherwise -> do
            window_ <- #{peek XAnyEvent, window} p
            return $ AnyEvent
                        { event_type    = type_
                        , serial        = serial_
                        , send_event    = send_event_
                        , event_display = display_
                        , window        = window_
                        }

data WindowChanges = WindowChanges
                        { wcX :: Int
                        , wcY :: Int
                        , wcWidth :: Int
                        , wcHeight:: Int
                        , wcBorderWidth :: Int
                        , wcSibling :: Window
                        , wcStackMode :: Int
                        }

instance Storable WindowChanges where
    sizeOf _ = #{size XWindowChanges}

    -- I really hope this is right:
    alignment _ = alignment (undefined :: Int)

    poke p wc = do
        #{poke XWindowChanges, x           } p $ wcX wc
        #{poke XWindowChanges, y           } p $ wcY wc
        #{poke XWindowChanges, width       } p $ wcWidth wc
        #{poke XWindowChanges, height      } p $ wcHeight wc
        #{poke XWindowChanges, border_width} p $ wcBorderWidth wc
        #{poke XWindowChanges, sibling     } p $ wcSibling wc
        #{poke XWindowChanges, stack_mode  } p $ wcStackMode wc
        
    peek p = return WindowChanges
                `ap` (#{peek XWindowChanges, x} p)
                `ap` (#{peek XWindowChanges, y} p)
                `ap` (#{peek XWindowChanges, width} p)
                `ap` (#{peek XWindowChanges, height} p)
                `ap` (#{peek XWindowChanges, border_width} p)
                `ap` (#{peek XWindowChanges, sibling} p)
                `ap` (#{peek XWindowChanges, stack_mode} p)

foreign import ccall unsafe "XlibExtras.h XConfigureWindow"
    xConfigureWindow :: Display -> Window -> CULong -> Ptr WindowChanges -> IO Int

configureWindow :: Display -> Window -> CULong -> WindowChanges -> IO ()
configureWindow d w m c = do
    with c (xConfigureWindow d w m)
    return ()
