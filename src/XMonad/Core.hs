{-# LANGUAGE ExistentialQuantification, FlexibleInstances, GeneralizedNewtypeDeriving,
             MultiParamTypeClasses, TypeSynonymInstances, DeriveDataTypeable,
             LambdaCase, NamedFieldPuns, DeriveTraversable #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Core
-- Copyright   :  (c) Spencer Janssen 2007
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  spencerjanssen@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, uses cunning newtype deriving
--
-- The 'X' monad, a state monad transformer over 'IO', for the window
-- manager state, and support routines.
--
-----------------------------------------------------------------------------

module XMonad.Core (
    X, WindowSet, WindowSpace, WorkspaceId,
    ScreenId(..), ScreenDetail(..), XState(..),
    XConf(..), XConfig(..), LayoutClass(..),
    Layout(..), readsLayout, Typeable, Message,
    SomeMessage(..), fromMessage, LayoutMessages(..),
    StateExtension(..), ExtensionClass(..), ConfExtension(..),
    runX, catchX, userCode, userCodeDef, io, catchIO, installSignalHandlers, uninstallSignalHandlers,
    withDisplay, withWindowSet, isRoot, runOnWorkspaces,
    getAtom, spawn, spawnPID, xfork, recompile, trace, whenJust, whenX,
    getXMonadDir, getXMonadCacheDir, getXMonadDataDir, stateFileName,
    atom_WM_STATE, atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_TAKE_FOCUS, withWindowAttributes,
    ManageHook, Query(..), runQuery, Directories'(..), Directories, getDirectories,
  ) where

import XMonad.StackSet hiding (modify)

import Prelude
import Control.Exception (fromException, try, bracket, throw, finally, SomeException(..))
import qualified Control.Exception as E
import Control.Applicative ((<|>), empty)
import Control.Monad.Fail
import Control.Monad.State
import Control.Monad.Reader
import Data.Semigroup
import Data.Traversable (for)
import Data.Default
import System.FilePath
import System.IO
import System.Info
import System.Posix.Env (getEnv)
import System.Posix.Process (executeFile, forkProcess, getAnyProcessStatus, createSession)
import System.Posix.Signals
import System.Posix.IO
import System.Posix.Types (ProcessID)
import System.Process
import System.Directory
import System.Exit
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras (getWindowAttributes, WindowAttributes, Event)
import Data.Typeable
import Data.List ((\\))
import Data.Maybe (isJust,fromMaybe)

import qualified Data.Map as M
import qualified Data.Set as S

-- | XState, the (mutable) window manager state.
data XState = XState
    { windowset        :: !WindowSet                     -- ^ workspace list
    , mapped           :: !(S.Set Window)                -- ^ the Set of mapped windows
    , waitingUnmap     :: !(M.Map Window Int)            -- ^ the number of expected UnmapEvents
    , dragging         :: !(Maybe (Position -> Position -> X (), X ()))
    , numberlockMask   :: !KeyMask                       -- ^ The numlock modifier
    , extensibleState  :: !(M.Map String (Either String StateExtension))
    -- ^ stores custom state information.
    --
    -- The module "XMonad.Util.ExtensibleState" in xmonad-contrib
    -- provides additional information and a simple interface for using this.
    }

-- | XConf, the (read-only) window manager configuration.
data XConf = XConf
    { display       :: Display        -- ^ the X11 display
    , config        :: !(XConfig Layout)       -- ^ initial user configuration
    , theRoot       :: !Window        -- ^ the root window
    , normalBorder  :: !Pixel         -- ^ border color of unfocused windows
    , focusedBorder :: !Pixel         -- ^ border color of the focused window
    , keyActions    :: !(M.Map (KeyMask, KeySym) (X ()))
                                      -- ^ a mapping of key presses to actions
    , buttonActions :: !(M.Map (KeyMask, Button) (Window -> X ()))
                                      -- ^ a mapping of button presses to actions
    , mouseFocused :: !Bool           -- ^ was refocus caused by mouse action?
    , mousePosition :: !(Maybe (Position, Position))
                                      -- ^ position of the mouse according to
                                      -- the event currently being processed
    , currentEvent :: !(Maybe Event)  -- ^ event currently being processed
    , directories  :: !Directories    -- ^ directories to use
    }

-- todo, better name
data XConfig l = XConfig
    { normalBorderColor  :: !String              -- ^ Non focused windows border color. Default: \"#dddddd\"
    , focusedBorderColor :: !String              -- ^ Focused windows border color. Default: \"#ff0000\"
    , terminal           :: !String              -- ^ The preferred terminal application. Default: \"xterm\"
    , layoutHook         :: !(l Window)          -- ^ The available layouts
    , manageHook         :: !ManageHook          -- ^ The action to run when a new window is opened
    , handleEventHook    :: !(Event -> X All)    -- ^ Handle an X event, returns (All True) if the default handler
                                                 -- should also be run afterwards. mappend should be used for combining
                                                 -- event hooks in most cases.
    , workspaces         :: ![String]            -- ^ The list of workspaces' names
    , modMask            :: !KeyMask             -- ^ the mod modifier
    , keys               :: !(XConfig Layout -> M.Map (ButtonMask,KeySym) (X ()))
                                                 -- ^ The key binding: a map from key presses and actions
    , mouseBindings      :: !(XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ()))
                                                 -- ^ The mouse bindings
    , borderWidth        :: !Dimension           -- ^ The border width
    , logHook            :: !(X ())              -- ^ The action to perform when the windows set is changed
    , startupHook        :: !(X ())              -- ^ The action to perform on startup
    , focusFollowsMouse  :: !Bool                -- ^ Whether window entry events can change focus
    , clickJustFocuses   :: !Bool                -- ^ False to make a click which changes focus to be additionally passed to the window
    , clientMask         :: !EventMask           -- ^ The client events that xmonad is interested in
    , rootMask           :: !EventMask           -- ^ The root events that xmonad is interested in
    , handleExtraArgs    :: !([String] -> XConfig Layout -> IO (XConfig Layout))
                                                 -- ^ Modify the configuration, complain about extra arguments etc. with arguments that are not handled by default
    , extensibleConf     :: !(M.Map TypeRep ConfExtension)
                                                 -- ^ Stores custom config information.
                                                 --
                                                 -- The module "XMonad.Util.ExtensibleConf" in xmonad-contrib
                                                 -- provides additional information and a simple interface for using this.
    }


type WindowSet   = StackSet  WorkspaceId (Layout Window) Window ScreenId ScreenDetail
type WindowSpace = Workspace WorkspaceId (Layout Window) Window

-- | Virtual workspace indices
type WorkspaceId = String

-- | Physical screen indices
newtype ScreenId    = S Int deriving (Eq,Ord,Show,Read,Enum,Num,Integral,Real)

-- | The 'Rectangle' with screen dimensions
newtype ScreenDetail = SD { screenRect :: Rectangle }
    deriving (Eq,Show, Read)

------------------------------------------------------------------------

-- | The X monad, 'ReaderT' and 'StateT' transformers over 'IO'
-- encapsulating the window manager configuration and state,
-- respectively.
--
-- Dynamic components may be retrieved with 'get', static components
-- with 'ask'. With newtype deriving we get readers and state monads
-- instantiated on 'XConf' and 'XState' automatically.
--
newtype X a = X (ReaderT XConf (StateT XState IO) a)
    deriving (Functor, Monad, MonadFail, MonadIO, MonadState XState, MonadReader XConf, Typeable)

instance Applicative X where
  pure = return
  (<*>) = ap

instance Semigroup a => Semigroup (X a) where
    (<>) = liftM2 (<>)

instance (Monoid a) => Monoid (X a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Default a => Default (X a) where
    def = return def

type ManageHook = Query (Endo WindowSet)
newtype Query a = Query (ReaderT Window X a)
    deriving (Functor, Applicative, Monad, MonadReader Window, MonadIO)

runQuery :: Query a -> Window -> X a
runQuery (Query m) w = runReaderT m w

instance Semigroup a => Semigroup (Query a) where
    (<>) = liftM2 (<>)

instance Monoid a => Monoid (Query a) where
    mempty  = return mempty
    mappend = liftM2 mappend

instance Default a => Default (Query a) where
    def = return def

-- | Run the 'X' monad, given a chunk of 'X' monad code, and an initial state
-- Return the result, and final state
runX :: XConf -> XState -> X a -> IO (a, XState)
runX c st (X a) = runStateT (runReaderT a c) st

-- | Run in the 'X' monad, and in case of exception, and catch it and log it
-- to stderr, and run the error case.
catchX :: X a -> X a -> X a
catchX job errcase = do
    st <- get
    c <- ask
    (a, s') <- io $ runX c st job `E.catch` \e -> case fromException e of
                        Just x -> throw e `const` (x `asTypeOf` ExitSuccess)
                        _ -> do hPrint stderr e; runX c st errcase
    put s'
    return a

-- | Execute the argument, catching all exceptions.  Either this function or
-- 'catchX' should be used at all callsites of user customized code.
userCode :: X a -> X (Maybe a)
userCode a = catchX (Just `liftM` a) (return Nothing)

-- | Same as userCode but with a default argument to return instead of using
-- Maybe, provided for convenience.
userCodeDef :: a -> X a -> X a
userCodeDef defValue a = fromMaybe defValue `liftM` userCode a

-- ---------------------------------------------------------------------
-- Convenient wrappers to state

-- | Run a monad action with the current display settings
withDisplay :: (Display -> X a) -> X a
withDisplay   f = asks display >>= f

-- | Run a monadic action with the current stack set
withWindowSet :: (WindowSet -> X a) -> X a
withWindowSet f = gets windowset >>= f

-- | Safely access window attributes.
withWindowAttributes :: Display -> Window -> (WindowAttributes -> X ()) -> X ()
withWindowAttributes dpy win f = do
    wa <- userCode (io $ getWindowAttributes dpy win)
    catchX (whenJust wa f) (return ())

-- | True if the given window is the root window
isRoot :: Window -> X Bool
isRoot w = (w==) <$> asks theRoot

-- | Wrapper for the common case of atom internment
getAtom :: String -> X Atom
getAtom str = withDisplay $ \dpy -> io $ internAtom dpy str False

-- | Common non-predefined atoms
atom_WM_PROTOCOLS, atom_WM_DELETE_WINDOW, atom_WM_STATE, atom_WM_TAKE_FOCUS :: X Atom
atom_WM_PROTOCOLS       = getAtom "WM_PROTOCOLS"
atom_WM_DELETE_WINDOW   = getAtom "WM_DELETE_WINDOW"
atom_WM_STATE           = getAtom "WM_STATE"
atom_WM_TAKE_FOCUS      = getAtom "WM_TAKE_FOCUS"

------------------------------------------------------------------------
-- LayoutClass handling. See particular instances in Operations.hs

-- | An existential type that can hold any object that is in 'Read'
--   and 'LayoutClass'.
data Layout a = forall l. (LayoutClass l a, Read (l a)) => Layout (l a)

-- | Using the 'Layout' as a witness, parse existentially wrapped windows
-- from a 'String'.
readsLayout :: Layout a -> String -> [(Layout a, String)]
readsLayout (Layout l) s = [(Layout (asTypeOf x l), rs) | (x, rs) <- reads s]

-- | Every layout must be an instance of 'LayoutClass', which defines
-- the basic layout operations along with a sensible default for each.
--
-- All of the methods have default implementations, so there is no
-- minimal complete definition.  They do, however, have a dependency
-- structure by default; this is something to be aware of should you
-- choose to implement one of these methods.  Here is how a minimal
-- complete definition would look like if we did not provide any default
-- implementations:
--
-- * 'runLayout' || (('doLayout' || 'pureLayout') && 'emptyLayout')
--
-- * 'handleMessage' || 'pureMessage'
--
-- * 'description'
--
-- Note that any code which /uses/ 'LayoutClass' methods should only
-- ever call 'runLayout', 'handleMessage', and 'description'!  In
-- other words, the only calls to 'doLayout', 'pureMessage', and other
-- such methods should be from the default implementations of
-- 'runLayout', 'handleMessage', and so on.  This ensures that the
-- proper methods will be used, regardless of the particular methods
-- that any 'LayoutClass' instance chooses to define.
class (Show (layout a), Typeable layout) => LayoutClass layout a where

    -- | By default, 'runLayout' calls 'doLayout' if there are any
    --   windows to be laid out, and 'emptyLayout' otherwise.  Most
    --   instances of 'LayoutClass' probably do not need to implement
    --   'runLayout'; it is only useful for layouts which wish to make
    --   use of more of the 'Workspace' information (for example,
    --   "XMonad.Layout.PerWorkspace").
    runLayout :: Workspace WorkspaceId (layout a) a
              -> Rectangle
              -> X ([(a, Rectangle)], Maybe (layout a))
    runLayout (Workspace _ l ms) r = maybe (emptyLayout l r) (doLayout l r) ms

    -- | Given a 'Rectangle' in which to place the windows, and a 'Stack'
    -- of windows, return a list of windows and their corresponding
    -- Rectangles.  If an element is not given a Rectangle by
    -- 'doLayout', then it is not shown on screen.  The order of
    -- windows in this list should be the desired stacking order.
    --
    -- Also possibly return a modified layout (by returning @Just
    -- newLayout@), if this layout needs to be modified (e.g. if it
    -- keeps track of some sort of state).  Return @Nothing@ if the
    -- layout does not need to be modified.
    --
    -- Layouts which do not need access to the 'X' monad ('IO', window
    -- manager state, or configuration) and do not keep track of their
    -- own state should implement 'pureLayout' instead of 'doLayout'.
    doLayout    :: layout a -> Rectangle -> Stack a
                -> X ([(a, Rectangle)], Maybe (layout a))
    doLayout l r s   = return (pureLayout l r s, Nothing)

    -- | This is a pure version of 'doLayout', for cases where we
    -- don't need access to the 'X' monad to determine how to lay out
    -- the windows, and we don't need to modify the layout itself.
    pureLayout  :: layout a -> Rectangle -> Stack a -> [(a, Rectangle)]
    pureLayout _ r s = [(focus s, r)]

    -- | 'emptyLayout' is called when there are no windows.
    emptyLayout :: layout a -> Rectangle -> X ([(a, Rectangle)], Maybe (layout a))
    emptyLayout _ _ = return ([], Nothing)

    -- | 'handleMessage' performs message handling.  If
    -- 'handleMessage' returns @Nothing@, then the layout did not
    -- respond to the message and the screen is not refreshed.
    -- Otherwise, 'handleMessage' returns an updated layout and the
    -- screen is refreshed.
    --
    -- Layouts which do not need access to the 'X' monad to decide how
    -- to handle messages should implement 'pureMessage' instead of
    -- 'handleMessage' (this restricts the risk of error, and makes
    -- testing much easier).
    handleMessage :: layout a -> SomeMessage -> X (Maybe (layout a))
    handleMessage l  = return . pureMessage l

    -- | Respond to a message by (possibly) changing our layout, but
    -- taking no other action.  If the layout changes, the screen will
    -- be refreshed.
    pureMessage :: layout a -> SomeMessage -> Maybe (layout a)
    pureMessage _ _  = Nothing

    -- | This should be a human-readable string that is used when
    -- selecting layouts by name.  The default implementation is
    -- 'show', which is in some cases a poor default.
    description :: layout a -> String
    description      = show

instance LayoutClass Layout Window where
    runLayout (Workspace i (Layout l) ms) r = fmap (fmap Layout) `fmap` runLayout (Workspace i l ms) r
    doLayout (Layout l) r s  = fmap (fmap Layout) `fmap` doLayout l r s
    emptyLayout (Layout l) r = fmap (fmap Layout) `fmap` emptyLayout l r
    handleMessage (Layout l) = fmap (fmap Layout) . handleMessage l
    description (Layout l)   = description l

instance Show (Layout a) where show (Layout l) = show l

-- | Based on ideas in /An Extensible Dynamically-Typed Hierarchy of
-- Exceptions/, Simon Marlow, 2006. Use extensible messages to the
-- 'handleMessage' handler.
--
-- User-extensible messages must be a member of this class.
--
class Typeable a => Message a

-- |
-- A wrapped value of some type in the 'Message' class.
--
data SomeMessage = forall a. Message a => SomeMessage a

-- |
-- And now, unwrap a given, unknown 'Message' type, performing a (dynamic)
-- type check on the result.
--
fromMessage :: Message m => SomeMessage -> Maybe m
fromMessage (SomeMessage m) = cast m

-- X Events are valid Messages.
instance Message Event

-- | 'LayoutMessages' are core messages that all layouts (especially stateful
-- layouts) should consider handling.
data LayoutMessages = Hide              -- ^ sent when a layout becomes non-visible
                    | ReleaseResources  -- ^ sent when xmonad is exiting or restarting
    deriving (Typeable, Eq)

instance Message LayoutMessages

-- ---------------------------------------------------------------------
-- Extensible state/config
--

-- | Every module must make the data it wants to store
-- an instance of this class.
--
-- Minimal complete definition: initialValue
class Typeable a => ExtensionClass a where
    {-# MINIMAL initialValue #-}
    -- | Defines an initial value for the state extension
    initialValue :: a
    -- | Specifies whether the state extension should be
    -- persistent. Setting this method to 'PersistentExtension'
    -- will make the stored data survive restarts, but
    -- requires a to be an instance of Read and Show.
    --
    -- It defaults to 'StateExtension', i.e. no persistence.
    extensionType :: a -> StateExtension
    extensionType = StateExtension

-- | Existential type to store a state extension.
data StateExtension =
    forall a. ExtensionClass a => StateExtension a
    -- ^ Non-persistent state extension
  | forall a. (Read a, Show a, ExtensionClass a) => PersistentExtension a
    -- ^ Persistent extension

-- | Existential type to store a config extension.
data ConfExtension = forall a. Typeable a => ConfExtension a

-- ---------------------------------------------------------------------
-- | General utilities
--
-- Lift an 'IO' action into the 'X' monad
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Lift an 'IO' action into the 'X' monad.  If the action results in an 'IO'
-- exception, log the exception to stderr and continue normal execution.
catchIO :: MonadIO m => IO () -> m ()
catchIO f = io (f `E.catch` \(SomeException e) -> hPrint stderr e >> hFlush stderr)

-- | spawn. Launch an external application. Specifically, it double-forks and
-- runs the 'String' you pass as a command to \/bin\/sh.
--
-- Note this function assumes your locale uses utf8.
spawn :: MonadIO m => String -> m ()
spawn x = spawnPID x >> return ()

-- | Like 'spawn', but returns the 'ProcessID' of the launched application
spawnPID :: MonadIO m => String -> m ProcessID
spawnPID x = xfork $ executeFile "/bin/sh" False ["-c", x] Nothing

-- | A replacement for 'forkProcess' which resets default signal handlers.
xfork :: MonadIO m => IO () -> m ProcessID
xfork x = io . forkProcess . finally nullStdin $ do
                uninstallSignalHandlers
                createSession
                x
 where
    nullStdin = do
        fd <- openFd "/dev/null" ReadOnly Nothing defaultFileFlags
        dupTo fd stdInput
        closeFd fd

-- | This is basically a map function, running a function in the 'X' monad on
-- each workspace with the output of that function being the modified workspace.
runOnWorkspaces :: (WindowSpace -> X WindowSpace) -> X ()
runOnWorkspaces job = do
    ws <- gets windowset
    h <- mapM job $ hidden ws
    c:v <- mapM (\s -> (\w -> s { workspace = w}) <$> job (workspace s))
             $ current ws : visible ws
    modify $ \s -> s { windowset = ws { current = c, visible = v, hidden = h } }

-- | All the directories that xmonad will use.  They will be used for
-- the following purposes:
--
-- * @dataDir@: This directory is used by XMonad to store data files
-- such as the run-time state file and the configuration binary
-- generated by GHC.
--
-- * @cfgDir@: This directory is where user configuration files are
-- stored (e.g, the xmonad.hs file).  You may also create a @lib@
-- subdirectory in the configuration directory and the default recompile
-- command will add it to the GHC include path.
--
-- * @cacheDir@: This directory is used to store temporary files that
-- can easily be recreated.  For example, the XPrompt history file.
--
-- For how these directories are chosen, see 'getDirectories'.
--
data Directories' a = Directories
    { dataDir  :: !a
    , cfgDir   :: !a
    , cacheDir :: !a
    }
    deriving (Show, Functor, Foldable, Traversable)

-- | Convenient type alias for the most common case in which one might
-- want to use the 'Directories' type.
type Directories = Directories' FilePath

-- | Build up the 'Dirs' that xmonad will use.  They are chosen as
-- follows:
--
-- 1. If all three of xmonad's environment variables (@XMONAD_DATA_DIR@,
--    @XMONAD_CONFIG_DIR@, and @XMONAD_CACHE_DIR@) are set, use them.
-- 2. If there is a build script called @build@ or configuration
--    @xmonad.hs@ in @~\/.xmonad@, set all three directories to
--    @~\/.xmonad@.
-- 3. Otherwise, use the @xmonad@ directory in @XDG_DATA_HOME@,
--    @XDG_CONFIG_HOME@, and @XDG_CACHE_HOME@ (or their respective
--    fallbacks).  These directories are created if necessary.
--
-- The xmonad configuration file (or the build script, if present) is
-- always assumed to be in @cfgDir@.
--
getDirectories :: IO Directories
getDirectories = xmEnvDirs <|> xmDirs <|> xdgDirs
  where
    -- | Check for xmonad's environment variables first
    xmEnvDirs :: IO Directories
    xmEnvDirs = do
        let xmEnvs = Directories{ dataDir  = "XMONAD_DATA_DIR"
                         , cfgDir   = "XMONAD_CONFIG_DIR"
                         , cacheDir = "XMONAD_CACHE_DIR"
                         }
        maybe empty pure . sequenceA =<< traverse getEnv xmEnvs

    -- | Check whether the config file or a build script is in the
    -- @~\/.xmonad@ directory
    xmDirs :: IO Directories
    xmDirs = do
        xmDir <- getAppUserDataDirectory "xmonad"
        conf  <- doesFileExist $ xmDir </> "xmonad.hs"
        build <- doesFileExist $ xmDir </> "build"

        -- Place *everything* in ~/.xmonad if yes
        guard $ conf || build
        pure Directories{ dataDir = xmDir, cfgDir = xmDir, cacheDir = xmDir }

    -- | Use XDG directories as a fallback
    xdgDirs :: IO Directories
    xdgDirs =
        for Directories{ dataDir = XdgData, cfgDir = XdgConfig, cacheDir = XdgCache }
            $ \dir -> do d <- getXdgDirectory dir "xmonad"
                         d <$ createDirectoryIfMissing True d

-- | Return the path to the xmonad configuration directory.
getXMonadDir :: X String
getXMonadDir = asks (cfgDir . directories)
{-# DEPRECATED getXMonadDir "Use `asks (cfgDir . directories)' instead." #-}

-- | Return the path to the xmonad cache directory.
getXMonadCacheDir :: X String
getXMonadCacheDir = asks (cacheDir . directories)
{-# DEPRECATED getXMonadCacheDir "Use `asks (cacheDir . directories)' instead." #-}

-- | Return the path to the xmonad data directory.
getXMonadDataDir :: X String
getXMonadDataDir = asks (dataDir . directories)
{-# DEPRECATED getXMonadDataDir "Use `asks (dataDir . directories)' instead." #-}

-- | Get the name of the file used to store the xmonad window state.
stateFileName :: X FilePath
stateFileName = (</> "xmonad.state") <$> getXMonadDataDir

-- | 'recompile force', recompile the xmonad configuration file when
-- any of the following apply:
--
--      * force is 'True'
--
--      * the xmonad executable does not exist
--
--      * the xmonad executable is older than xmonad.hs or any file in
--        the @lib@ directory (under the configuration directory).
--
-- The -i flag is used to restrict recompilation to the xmonad.hs file only,
-- and any files in the aforementioned @lib@ directory.
--
-- Compilation errors (if any) are logged to the @xmonad.errors@ file
-- in the xmonad data directory.  If GHC indicates failure with a
-- non-zero exit code, an xmessage displaying that file is spawned.
--
-- 'False' is returned if there are compilation errors.
--
recompile :: MonadIO m => Directories -> Bool -> m Bool
recompile Directories{ cfgDir, dataDir } force = io $ do
    let binn = "xmonad-"++arch++"-"++os
        bin  = dataDir </> binn
        err  = dataDir </> "xmonad.errors"
        src  = cfgDir  </> "xmonad.hs"
        lib  = cfgDir  </> "lib"
        buildscript = cfgDir </> "build"

    libTs <- mapM getModTime . Prelude.filter isSource =<< allFiles lib
    srcT <- getModTime src
    binT <- getModTime bin

    useBuildscript <- do
      exists <- doesFileExist buildscript
      if exists
        then do
          isExe <- isExecutable buildscript
          if isExe
            then do
              trace $ "XMonad will use build script at " ++ show buildscript ++ " to recompile."
              return True
            else do
              trace $ unlines
                [ "XMonad will not use build script, because " ++ show buildscript ++ " is not executable."
                , "Suggested resolution to use it: chmod u+x " ++ show buildscript
                ]
              return False
        else do
          trace $
            "XMonad will use ghc to recompile, because " ++ show buildscript ++ " does not exist."
          return False

    shouldRecompile <-
      if useBuildscript || force
        then return True
        else if any (binT <) (srcT : libTs)
          then do
            trace "XMonad doing recompile because some files have changed."
            return True
          else do
            trace "XMonad skipping recompile because it is not forced (e.g. via --recompile), and neither xmonad.hs nor any *.hs / *.lhs / *.hsc files in lib/ have been changed."
            return False

    if shouldRecompile
      then do
        -- temporarily disable SIGCHLD ignoring:
        uninstallSignalHandlers
        status <- bracket (openFile err WriteMode) hClose $ \errHandle ->
            waitForProcess =<< if useBuildscript
                               then compileScript bin cfgDir buildscript errHandle
                               else compileGHC bin cfgDir errHandle

        -- re-enable SIGCHLD:
        installSignalHandlers

        -- now, if it fails, run xmessage to let the user know:
        if status == ExitSuccess
            then trace "XMonad recompilation process exited with success!"
            else do
                ghcErr <- readFile err
                let msg = unlines $
                        ["Error detected while loading xmonad configuration file: " ++ src]
                        ++ lines (if null ghcErr then show status else ghcErr)
                        ++ ["","Please check the file for errors."]
                -- nb, the ordering of printing, then forking, is crucial due to
                -- lazy evaluation
                hPutStrLn stderr msg
                forkProcess $ executeFile "xmessage" True ["-default", "okay", replaceUnicode msg] Nothing
                return ()
        return (status == ExitSuccess)
      else return True
 where getModTime f = E.catch (Just <$> getModificationTime f) (\(SomeException _) -> return Nothing)
       isSource = flip elem [".hs",".lhs",".hsc"] . takeExtension
       isExecutable f = E.catch (executable <$> getPermissions f) (\(SomeException _) -> return False)
       allFiles t = do
            let prep = map (t</>) . Prelude.filter (`notElem` [".",".."])
            cs <- prep <$> E.catch (getDirectoryContents t) (\(SomeException _) -> return [])
            ds <- filterM doesDirectoryExist cs
            concat . ((cs \\ ds):) <$> mapM allFiles ds
       -- Replace some of the unicode symbols GHC uses in its output
       replaceUnicode = map $ \c -> case c of
           '\8226' -> '*'  -- •
           '\8216' -> '`'  -- ‘
           '\8217' -> '`'  -- ’
           _ -> c
       compileGHC bin dir errHandle =
         runProcess "ghc" ["--make"
                          , "xmonad.hs"
                          , "-i"
                          , "-ilib"
                          , "-fforce-recomp"
                          , "-main-is", "main"
                          , "-v0"
                          , "-o", bin
                          ] (Just dir) Nothing Nothing Nothing (Just errHandle)
       compileScript bin dir script errHandle =
         runProcess script [bin] (Just dir) Nothing Nothing Nothing (Just errHandle)

-- | Conditionally run an action, using a @Maybe a@ to decide.
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust mg f = maybe (return ()) f mg

-- | Conditionally run an action, using a 'X' event to decide
whenX :: X Bool -> X () -> X ()
whenX a f = a >>= \b -> when b f

-- | A 'trace' for the 'X' monad. Logs a string to stderr. The result may
-- be found in your .xsession-errors file
trace :: MonadIO m => String -> m ()
trace = io . hPutStrLn stderr

-- | Ignore SIGPIPE to avoid termination when a pipe is full, and SIGCHLD to
-- avoid zombie processes, and clean up any extant zombie processes.
installSignalHandlers :: MonadIO m => m ()
installSignalHandlers = io $ do
    installHandler openEndedPipe Ignore Nothing
    installHandler sigCHLD Ignore Nothing
    (try :: IO a -> IO (Either SomeException a))
      $ fix $ \more -> do
        x <- getAnyProcessStatus False False
        when (isJust x) more
    return ()

uninstallSignalHandlers :: MonadIO m => m ()
uninstallSignalHandlers = io $ do
    installHandler openEndedPipe Default Nothing
    installHandler sigCHLD Default Nothing
    return ()
