{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Main
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses mtl, X11, posix
--
-- xmonad, a minimalist, tiling window manager for X11
--
-----------------------------------------------------------------------------

module XMonad.Main (xmonad, launch) where

import System.Locale.SetLocale
import qualified Control.Exception.Extensible as E
import Data.Bits
import Data.List ((\\))
import Data.Function
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader
import Control.Monad.State
import Data.Maybe (fromMaybe)
import Data.Monoid (getAll)

import Graphics.X11.Xlib hiding (refreshKeyboardMapping)
import Graphics.X11.Xlib.Extras

import XMonad.Core
import qualified XMonad.Config as Default
import XMonad.StackSet (new, floating, member)
import qualified XMonad.StackSet as W
import XMonad.Operations

import System.IO
import System.Directory
import System.Info
import System.Environment
import System.Posix.Process (executeFile)
import System.Exit (exitFailure)
import System.FilePath

import Paths_xmonad (version)
import Data.Version (showVersion)

import Graphics.X11.Xinerama (compiledWithXinerama)

------------------------------------------------------------------------


-- |
-- | The entry point into xmonad. Attempts to compile any custom main
-- for xmonad, and if it doesn't find one, just launches the default.
xmonad :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
xmonad conf = do
    installSignalHandlers -- important to ignore SIGCHLD to avoid zombies

    let launch' args = do
              catchIO buildLaunch
              conf' @ XConfig { layoutHook = Layout l }
                  <- handleExtraArgs conf args conf{ layoutHook = Layout (layoutHook conf) }
              withArgs [] $ launch (conf' { layoutHook = l })

    args <- getArgs
    case args of
        ("--resume": ws : xs : args') -> migrateState ws xs >> launch' args'
        ["--help"]            -> usage
        ["--recompile"]       -> recompile True >>= flip unless exitFailure
        ["--restart"]         -> sendRestart
        ["--version"]         -> putStrLn $ unwords shortVersion
        ["--verbose-version"] -> putStrLn . unwords $ shortVersion ++ longVersion
        "--replace" : args'   -> sendReplace >> launch' args'
        _                     -> launch' args
 where
    shortVersion = ["xmonad", showVersion version]
    longVersion  = [ "compiled by", compilerName, showVersion compilerVersion
                   , "for",  arch ++ "-" ++ os
                   , "\nXinerama:", show compiledWithXinerama ]


usage :: IO ()
usage = do
    self <- getProgName
    putStr . unlines $
        concat ["Usage: ", self, " [OPTION]"] :
        "Options:" :
        "  --help                       Print this message" :
        "  --version                    Print the version number" :
        "  --recompile                  Recompile your ~/.xmonad/xmonad.hs" :
        "  --replace                    Replace the running window manager with xmonad" :
        "  --restart                    Request a running xmonad process to restart" :
        []

-- | Build the xmonad configuration file with ghc, then execute it.
-- If there are no errors, this function does not return.  An
-- exception is raised in any of these cases:
--
--   * ghc missing
--
--   * both the configuration file and executable are missing
--
--   * xmonad.hs fails to compile
--
--      ** wrong ghc in path (fails to compile)
--
--      ** type error, syntax error, ..
--
--   * Missing XMonad\/XMonadContrib modules due to ghc upgrade
--
buildLaunch ::  IO ()
buildLaunch = do
    recompile False
    dir  <- getXMonadDataDir
    args <- getArgs
    whoami <- getProgName
    let compiledConfig = "xmonad-"++arch++"-"++os
    unless (whoami == compiledConfig) $
      executeFile (dir </> compiledConfig) False args Nothing

sendRestart :: IO ()
sendRestart = do
    dpy <- openDisplay ""
    rw <- rootWindow dpy $ defaultScreen dpy
    xmonad_restart <- internAtom dpy "XMONAD_RESTART" False
    allocaXEvent $ \e -> do
        setEventType e clientMessage
        setClientMessageEvent e rw xmonad_restart 32 0 currentTime
        sendEvent dpy rw False structureNotifyMask e
    sync dpy False

-- | a wrapper for 'replace'
sendReplace :: IO ()
sendReplace = do
    dpy <- openDisplay ""
    let dflt = defaultScreen dpy
    rootw  <- rootWindow dpy dflt
    replace dpy dflt rootw

-- | Entry point into xmonad for custom builds.
--
-- This function isn't meant to be called by the typical xmonad user
-- because it:
--
--   * Does not process any command line arguments.
--
--   * Therefore doesn't know how to restart a running xmonad.
--
--   * Does not compile your configuration file since it assumes it's
--     actually running from within your compiled configuration.
--
-- Unless you know what you are doing, you should probably be using
-- the 'xmonad' function instead.
--
-- However, if you are using a custom build environment (such as
-- stack, cabal, make, etc.) you will likely want to call this
-- function instead of 'xmonad'.  You probably also want to have a key
-- binding to the 'XMonad.Operations.restart` function that restarts
-- your custom binary with the resume flag set to @True@.
launch :: (LayoutClass l Window, Read (l Window)) => XConfig l -> IO ()
launch initxmc = do
    -- setup locale information from environment
    setLocale LC_ALL (Just "")
    -- ignore SIGPIPE and SIGCHLD
    installSignalHandlers
    -- First, wrap the layout in an existential, to keep things pretty:
    let xmc = initxmc { layoutHook = Layout $ layoutHook initxmc }
    dpy   <- openDisplay ""
    let dflt = defaultScreen dpy

    rootw  <- rootWindow dpy dflt

    -- If another WM is running, a BadAccess error will be returned.  The
    -- default error handler will write the exception to stderr and exit with
    -- an error.
    selectInput dpy rootw $ rootMask initxmc

    sync dpy False -- sync to ensure all outstanding errors are delivered

    -- turn off the default handler in favor of one that ignores all errors
    -- (ugly, I know)
    xSetErrorHandler -- in C, I'm too lazy to write the binding: dons

    nbc    <- do v            <- initColor dpy $ normalBorderColor  xmc
                 ~(Just nbc_) <- initColor dpy $ normalBorderColor Default.def
                 return (fromMaybe nbc_ v)

    fbc    <- do v <- initColor dpy $ focusedBorderColor xmc
                 ~(Just fbc_)  <- initColor dpy $ focusedBorderColor Default.def
                 return (fromMaybe fbc_ v)

    hSetBuffering stdout NoBuffering
    xinesc <- getCleanedScreenInfo dpy

    let layout = layoutHook xmc
        lreads = readsLayout layout
        initialWinset = let padToLen n xs = take (max n (length xs)) $ xs ++ repeat ""
            in new layout (padToLen (length xinesc) (workspaces xmc)) $ map SD xinesc

        cf = XConf
            { display       = dpy
            , config        = xmc
            , theRoot       = rootw
            , normalBorder  = nbc
            , focusedBorder = fbc
            , keyActions    = keys xmc xmc
            , buttonActions = mouseBindings xmc xmc
            , mouseFocused  = False
            , mousePosition = Nothing
            , currentEvent  = Nothing }

        st = XState
            { windowset       = initialWinset
            , numberlockMask  = 0
            , mapped          = S.empty
            , waitingUnmap    = M.empty
            , dragging        = Nothing
            , extensibleState = M.empty
            }

    allocaXEvent $ \e ->
        runX cf st $ do
            -- check for serialized state in a file.
            serializedSt <- do
                path <- stateFileName
                exists <- io (doesFileExist path)
                if exists then readStateFile initxmc else return Nothing

            -- restore extensibleState if we read it from a file.
            let extst = maybe M.empty extensibleState serializedSt
            modify (\s -> s {extensibleState = extst})

            setNumlockMask
            grabKeys
            grabButtons

            io $ sync dpy False

            ws <- io $ scan dpy rootw

            -- bootstrap the windowset, Operations.windows will identify all
            -- the windows in winset as new and set initial properties for
            -- those windows.  Remove all windows that are no longer top-level
            -- children of the root, they may have disappeared since
            -- restarting.
            let winset = maybe initialWinset windowset serializedSt
            windows . const . foldr W.delete winset $ W.allWindows winset \\ ws

            -- manage the as-yet-unmanaged windows
            mapM_ manage (ws \\ W.allWindows winset)

            userCode $ startupHook initxmc

            -- main loop, for all you HOF/recursion fans out there.
            forever $ prehandle =<< io (nextEvent dpy e >> getEvent e)

    return ()
      where
        -- if the event gives us the position of the pointer, set mousePosition
        prehandle e = let mouse = do guard (ev_event_type e `elem` evs)
                                     return (fromIntegral (ev_x_root e)
                                            ,fromIntegral (ev_y_root e))
                      in local (\c -> c { mousePosition = mouse, currentEvent = Just e }) (handleWithHook e)
        evs = [ keyPress, keyRelease, enterNotify, leaveNotify
              , buttonPress, buttonRelease]


-- | Runs handleEventHook from the configuration and runs the default handler
-- function if it returned True.
handleWithHook :: Event -> X ()
handleWithHook e = do
  evHook <- asks (handleEventHook . config)
  whenX (userCodeDef True $ getAll `fmap` evHook e) (handle e)

-- ---------------------------------------------------------------------
-- | Event handler. Map X events onto calls into Operations.hs, which
-- modify our internal model of the window manager state.
--
-- Events dwm handles that we don't:
--
--    [ButtonPress]    = buttonpress,
--    [Expose]         = expose,
--    [PropertyNotify] = propertynotify,
--
handle :: Event -> X ()

-- run window manager command
handle (KeyEvent {ev_event_type = t, ev_state = m, ev_keycode = code})
    | t == keyPress = withDisplay $ \dpy -> do
        s  <- io $ keycodeToKeysym dpy code 0
        mClean <- cleanMask m
        ks <- asks keyActions
        userCodeDef () $ whenJust (M.lookup (mClean, s) ks) id

-- manage a new window
handle (MapRequestEvent    {ev_window = w}) = withDisplay $ \dpy -> do
    withWindowAttributes dpy w $ \wa -> do -- ignore override windows
      -- need to ignore mapping requests by managed windows not on the current workspace
      managed <- isClient w
      when (not (wa_override_redirect wa) && not managed) $ manage w

-- window destroyed, unmanage it
-- window gone,      unmanage it
handle (DestroyWindowEvent {ev_window = w}) = whenX (isClient w) $ do
    unmanage w
    modify (\s -> s { mapped       = S.delete w (mapped s)
                    , waitingUnmap = M.delete w (waitingUnmap s)})

-- We track expected unmap events in waitingUnmap.  We ignore this event unless
-- it is synthetic or we are not expecting an unmap notification from a window.
handle (UnmapEvent {ev_window = w, ev_send_event = synthetic}) = whenX (isClient w) $ do
    e <- gets (fromMaybe 0 . M.lookup w . waitingUnmap)
    if (synthetic || e == 0)
        then unmanage w
        else modify (\s -> s { waitingUnmap = M.update mpred w (waitingUnmap s) })
 where mpred 1 = Nothing
       mpred n = Just $ pred n

-- set keyboard mapping
handle e@(MappingNotifyEvent {}) = do
    io $ refreshKeyboardMapping e
    when (ev_request e `elem` [mappingKeyboard, mappingModifier]) $ do
        setNumlockMask
        grabKeys

-- handle button release, which may finish dragging.
handle e@(ButtonEvent {ev_event_type = t})
    | t == buttonRelease = do
    drag <- gets dragging
    case drag of
        -- we're done dragging and have released the mouse:
        Just (_,f) -> modify (\s -> s { dragging = Nothing }) >> f
        Nothing    -> broadcastMessage e

-- handle motionNotify event, which may mean we are dragging.
handle e@(MotionEvent {ev_event_type = _t, ev_x = x, ev_y = y}) = do
    drag <- gets dragging
    case drag of
        Just (d,_) -> d (fromIntegral x) (fromIntegral y) -- we're dragging
        Nothing -> broadcastMessage e

-- click on an unfocused window, makes it focused on this workspace
handle e@(ButtonEvent {ev_window = w,ev_event_type = t,ev_button = b })
    | t == buttonPress = do
    -- If it's the root window, then it's something we
    -- grabbed in grabButtons. Otherwise, it's click-to-focus.
    dpy <- asks display
    isr <- isRoot w
    m <- cleanMask $ ev_state e
    mact <- asks (M.lookup (m, b) . buttonActions)
    case mact of
        Just act | isr -> act $ ev_subwindow e
        _              -> do
            focus w
            ctf <- asks (clickJustFocuses . config)
            unless ctf $ io (allowEvents dpy replayPointer currentTime)
    broadcastMessage e -- Always send button events.

-- entered a normal window: focus it if focusFollowsMouse is set to
-- True in the user's config.
handle e@(CrossingEvent {ev_window = w, ev_event_type = t})
    | t == enterNotify && ev_mode   e == notifyNormal
    = whenX (asks $ focusFollowsMouse . config) (focus w)

-- left a window, check if we need to focus root
handle e@(CrossingEvent {ev_event_type = t})
    | t == leaveNotify
    = do rootw <- asks theRoot
         when (ev_window e == rootw && not (ev_same_screen e)) $ setFocusX rootw

-- configure a window
handle e@(ConfigureRequestEvent {ev_window = w}) = withDisplay $ \dpy -> do
    ws <- gets windowset
    bw <- asks (borderWidth . config)

    if M.member w (floating ws)
        || not (member w ws)
        then do io $ configureWindow dpy w (ev_value_mask e) $ WindowChanges
                    { wc_x            = ev_x e
                    , wc_y            = ev_y e
                    , wc_width        = ev_width e
                    , wc_height       = ev_height e
                    , wc_border_width = fromIntegral bw
                    , wc_sibling      = ev_above e
                    , wc_stack_mode   = ev_detail e }
                when (member w ws) (float w)
        else withWindowAttributes dpy w $ \wa -> io $ allocaXEvent $ \ev -> do
                 setEventType ev configureNotify
                 setConfigureEvent ev w w
                     (wa_x wa) (wa_y wa) (wa_width wa)
                     (wa_height wa) (ev_border_width e) none (wa_override_redirect wa)
                 sendEvent dpy w False 0 ev
    io $ sync dpy False

-- configuration changes in the root may mean display settings have changed
handle (ConfigureEvent {ev_window = w}) = whenX (isRoot w) rescreen

-- property notify
handle event@(PropertyEvent { ev_event_type = t, ev_atom = a })
    | t == propertyNotify && a == wM_NAME = asks (logHook . config) >>= userCodeDef () >>
                                         broadcastMessage event

handle e@ClientMessageEvent { ev_message_type = mt } = do
    a <- getAtom "XMONAD_RESTART"
    if (mt == a)
        then restart "xmonad" True
        else broadcastMessage e

handle e = broadcastMessage e -- trace (eventName e) -- ignoring


-- ---------------------------------------------------------------------
-- IO stuff. Doesn't require any X state
-- Most of these things run only on startup (bar grabkeys)

-- | scan for any new windows to manage. If they're already managed,
-- this should be idempotent.
scan :: Display -> Window -> IO [Window]
scan dpy rootw = do
    (_, _, ws) <- queryTree dpy rootw
    filterM (\w -> ok w `E.catch` skip) ws
  -- TODO: scan for windows that are either 'IsViewable' or where WM_STATE ==
  -- Iconic
  where ok w = do wa <- getWindowAttributes dpy w
                  a  <- internAtom dpy "WM_STATE" False
                  p  <- getWindowProperty32 dpy a w
                  let ic = case p of
                            Just (3:_) -> True -- 3 for iconified
                            _          -> False
                  return $ not (wa_override_redirect wa)
                         && (wa_map_state wa == waIsViewable || ic)

        skip :: E.SomeException -> IO Bool
        skip _ = return False

setNumlockMask :: X ()
setNumlockMask = do
    dpy <- asks display
    ms <- io $ getModifierMapping dpy
    xs <- sequence [ do
                        ks <- io $ keycodeToKeysym dpy kc 0
                        if ks == xK_Num_Lock
                            then return (setBit 0 (fromIntegral m))
                            else return (0 :: KeyMask)
                        | (m, kcs) <- ms, kc <- kcs, kc /= 0]
    modify (\s -> s { numberlockMask = foldr (.|.) 0 xs })

-- | Grab the keys back
grabKeys :: X ()
grabKeys = do
    XConf { display = dpy, theRoot = rootw } <- ask
    let grab kc m = io $ grabKey dpy kc m rootw True grabModeAsync grabModeAsync
        (minCode, maxCode) = displayKeycodes dpy
        allCodes = [fromIntegral minCode .. fromIntegral maxCode]
    io $ ungrabKey dpy anyKey anyModifier rootw
    ks <- asks keyActions
    -- build a map from keysyms to lists of keysyms (doing what
    -- XGetKeyboardMapping would do if the X11 package bound it)
    syms <- forM allCodes $ \code -> io (keycodeToKeysym dpy code 0)
    let keysymMap = M.fromListWith (++) (zip syms [[code] | code <- allCodes])
        keysymToKeycodes sym = M.findWithDefault [] sym keysymMap
    forM_ (M.keys ks) $ \(mask,sym) ->
         forM_ (keysymToKeycodes sym) $ \kc ->
              mapM_ (grab kc . (mask .|.)) =<< extraModifiers

-- | XXX comment me
grabButtons :: X ()
grabButtons = do
    XConf { display = dpy, theRoot = rootw } <- ask
    let grab button mask = io $ grabButton dpy button mask rootw False buttonPressMask
                                           grabModeAsync grabModeSync none none
    io $ ungrabButton dpy anyButton anyModifier rootw
    ems <- extraModifiers
    ba <- asks buttonActions
    mapM_ (\(m,b) -> mapM_ (grab b . (m .|.)) ems) (M.keys $ ba)

-- | @replace@ to signals compliant window managers to exit.
replace :: Display -> ScreenNumber -> Window -> IO ()
replace dpy dflt rootw = do
    -- check for other WM
    wmSnAtom <- internAtom dpy ("WM_S" ++ show dflt) False
    currentWmSnOwner <- xGetSelectionOwner dpy wmSnAtom
    when (currentWmSnOwner /= 0) $ do
        -- prepare to receive destroyNotify for old WM
        selectInput dpy currentWmSnOwner structureNotifyMask

        -- create off-screen window
        netWmSnOwner <- allocaSetWindowAttributes $ \attributes -> do
            set_override_redirect attributes True
            set_event_mask attributes propertyChangeMask
            let screen = defaultScreenOfDisplay dpy
                visual = defaultVisualOfScreen screen
                attrmask = cWOverrideRedirect .|. cWEventMask
            createWindow dpy rootw (-100) (-100) 1 1 0 copyFromParent copyFromParent visual attrmask attributes

        -- try to acquire wmSnAtom, this should signal the old WM to terminate
        xSetSelectionOwner dpy wmSnAtom netWmSnOwner currentTime

        -- SKIPPED: check if we acquired the selection
        -- SKIPPED: send client message indicating that we are now the WM

        -- wait for old WM to go away
        fix $ \again -> do
            evt <- allocaXEvent $ \event -> do
                windowEvent dpy currentWmSnOwner structureNotifyMask event
                get_EventType event

            when (evt /= destroyNotify) again
