-- | Wiring: macOS notifications go into a queue, one worker thread turns
-- them into 'Event's, steps the pure core, and carries out its 'Effect's.
-- The main thread belongs to Cocoa.
module Kineo.Runtime
  ( run
  , loadConfig
  , defaultConfigPath
  ) where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM_, forever, unless, void, when)
import Data.IORef
import Data.IntMap.Strict (IntMap)
import Data.IntMap.Strict qualified as IntMap
import Data.Map.Strict qualified as Map
import Data.Text.IO qualified as T
import GHC.Clock (getMonotonicTime)
import Kineo.Animator (Animator)
import Kineo.Animator qualified as Animator
import Kineo.Command (Command (..), commandName)
import Kineo.Config (Config (..), decodeConfig, defaultConfig)
import Kineo.Core
import Kineo.Geometry (Rect (..))
import Kineo.Keys (Chord (..), carbonModifiers, renderChord)
import Kineo.Log (Logger)
import Kineo.Log qualified as Log
import Kineo.Platform (RawEvent (..))
import Kineo.Platform qualified as Platform
import Kineo.Remote qualified as Remote
import Kineo.Strip (WindowId)
import System.Directory (doesFileExist, getXdgDirectory, XdgDirectory (..))
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Posix.Signals (Handler (..), installHandler, sigINT, sigTERM)

data Input
  = Raw RawEvent
  | Remote Command
  | -- | Time to check whether the user has let go of a window they dragged.
    SettleDrag

data Env = Env
  { logger :: Logger
  , configPath :: FilePath
  , config :: IORef Config
  , queue :: TQueue Input
  , animator :: Animator
  , hotkeys :: IORef (IntMap Command)
  , lastDrag :: IORef Double
  }

defaultConfigPath :: IO FilePath
defaultConfigPath = (</> "kineo.toml") <$> getXdgDirectory XdgConfig "kineo"

-- | Read a config file. A missing file means the defaults.
loadConfig :: FilePath -> IO (Either [String] (Config, [String]))
loadConfig path = do
  exists <- doesFileExist path
  if not exists
    then pure (Right (defaultConfig, []))
    else decodeConfig <$> T.readFile path

-- | Run the window manager. Must be called on the main thread; never returns.
run :: Logger -> FilePath -> IO ()
run lg path = do
  Platform.initialise
  running <- Remote.alreadyRunning
  when running $ Log.err lg "Kineo is already running." >> exitFailure

  cfg <-
    loadConfig path >>= \case
      Right (c, warns) -> do
        Log.info lg ("config: " ++ path)
        mapM_ (Log.warn lg) warns
        pure c
      Left errs -> do
        Log.err lg ("config " ++ path ++ " is invalid, using defaults:")
        mapM_ (Log.err lg) errs
        pure defaultConfig
  waitForAccessibility lg

  q <- newTQueueIO
  let enqueue = atomically . writeTQueue q
  anim <- Animator.start cfg.animation (enqueue . Raw . RawWindowDestroyed)
  env <- Env lg path <$> newIORef cfg <*> pure q <*> pure anim <*> newIORef IntMap.empty <*> newIORef 0
  registerHotkeys env cfg

  Remote.serve (enqueue . Remote)
  forM_ [sigINT, sigTERM] $ \s -> installHandler s (Catch (enqueue (Remote Quit))) Nothing

  _ <- forkFinally (worker env emptyWorld) $ \r -> do
    Log.err lg ("worker stopped: " ++ either displayException (const "returned") r)
    Platform.quit 1
  Log.info lg "running"
  Platform.runLoop (enqueue . Raw)

waitForAccessibility :: Logger -> IO ()
waitForAccessibility lg = do
  trusted <- Platform.accessibilityTrusted True
  unless trusted $ do
    Log.warn lg "Kineo needs Accessibility access: System Settings > Privacy & Security > Accessibility."
    Log.warn lg "Waiting for permission..."
    let wait = Platform.accessibilityTrusted False >>= \ok -> unless ok (threadDelay 1000000 >> wait)
    wait
    Log.info lg "Accessibility access granted"

worker :: Env -> World -> IO ()
worker env = loop
  where
    loop w = do
      input <- atomically (readTQueue env.queue)
      r <- try @SomeException (handle env input w)
      case r of
        Right w' -> loop w'
        Left e -> Log.err env.logger ("while handling an event: " ++ displayException e) >> loop w

handle :: Env -> Input -> World -> IO World
handle env input w = do
  events <- sense env input w
  let go world [] = pure world
      go world (e : es) = do
        Log.debug env.logger (show e)
        cfg <- readIORef env.config
        let (world', effects) = step cfg e world
        forM_ (Map.keys (Map.difference world.windows world'.windows)) (Animator.forget env.animator)
        more <- concat <$> mapM (perform env world') effects
        go world' (more ++ es)
  go w events

-- | Turn raw input into core events, asking macOS for whatever the core
-- needs to know.
sense :: Env -> Input -> World -> IO [Event]
sense env input w = case input of
  Remote c -> pure [Command c]
  SettleDrag -> do
    now <- getMonotonicTime
    t <- readIORef env.lastDrag
    pure [Relayout | now - t >= 0.45]
  Raw raw -> case raw of
    RawWindowCreated wid -> appeared wid
    RawWindowDestroyed wid -> pure [WindowGone wid]
    RawWindowFocused wid
      -- Windows can finish becoming "standard" after they are created;
      -- give an unknown window another look when it takes focus.
      | Map.member wid w.windows -> pure [WindowFocused wid]
      | otherwise -> (++ [WindowFocused wid]) <$> appeared wid
    RawWindowMoved wid -> external wid False
    RawWindowResized wid -> external wid True
    RawWindowMinimized wid m -> pure [WindowMinimized wid m]
    RawAppTerminated pid -> pure [AppTerminated pid]
    RawAppHidden pid h -> pure [AppHidden pid h]
    RawSpaceChanged -> reconfigured
    RawDisplaysChanged -> reconfigured
    RawHotkey i -> do
      keys <- readIORef env.hotkeys
      pure (maybe [] (pure . Command) (IntMap.lookup i keys))
  where
    appeared wid = maybe [] (pure . WindowAppeared) <$> Platform.queryWindow wid

    reconfigured = do
      ds <- Platform.displays
      let wids = Map.keys w.windows
      sids <- Platform.windowSpaces wids
      pure [Reconfigured ds (Map.fromList (zip wids sids))]

    -- A window moved or resized. Echoes of our own moves are ignored; a
    -- resize by the user becomes the column's new width; a drag is undone
    -- once the user lets go.
    external :: WindowId -> Bool -> IO [Event]
    external wid resized
      | not (Map.member wid w.windows) = pure []
      | otherwise = do
          ours <- Animator.busy env.animator wid
          if ours
            then pure []
            else
              Platform.windowFrame wid >>= \case
                Nothing -> pure []
                Just r -> do
                  before <- Animator.lastSet env.animator wid
                  Animator.observed env.animator wid r
                  let widthChanged = maybe True (\p -> abs (p.w - r.w) > 2) before
                  if resized && widthChanged
                    then pure [WindowResized wid r.w]
                    else do
                      getMonotonicTime >>= writeIORef env.lastDrag
                      void . forkIO $ threadDelay 500000 >> atomically (writeTQueue env.queue SettleDrag)
                      pure []

-- | Carry out an effect. May produce follow-up events.
perform :: Env -> World -> Effect -> IO [Event]
perform env w = \case
  Arrange ps -> [] <$ Animator.setTargets env.animator ps
  FocusWindow wid -> [] <$ Platform.focusWindow wid
  ForgetFrames -> [] <$ Animator.forgetAll env.animator
  LoadConfig ->
    loadConfig env.configPath >>= \case
      Left errs -> do
        Log.err env.logger ("not reloading, " ++ env.configPath ++ " is invalid:")
        mapM_ (Log.err env.logger) errs
        pure []
      Right (cfg, warns) -> do
        mapM_ (Log.warn env.logger) warns
        writeIORef env.config cfg
        Animator.setConfig env.animator cfg.animation
        registerHotkeys env cfg
        Log.info env.logger "config reloaded"
        pure [Relayout]
  Shutdown -> do
    Log.info env.logger "quitting; bringing parked windows back on screen"
    cfg <- readIORef env.config
    Animator.placeNow (released cfg w)
    Platform.quit 0
    forever (threadDelay maxBound)

registerHotkeys :: Env -> Config -> IO ()
registerHotkeys env cfg = do
  let binds = Map.toList cfg.bindings
  writeIORef env.hotkeys (IntMap.fromList (zip [0 ..] (map snd binds)))
  Platform.setHotkeys [(fromIntegral c.key, fromIntegral (carbonModifiers c.modifiers)) | (c, _) <- binds]
  forM_ binds $ \(c, cmd) -> Log.debug env.logger (renderChord c ++ " -> " ++ commandName cmd)
