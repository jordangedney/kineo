-- | A typed, Haskell-shaped view of the macOS layer in cbits/.
module Kineo.Platform
  ( RawEvent (..)
  , SetResult (..)
  , accessibilityTrusted
  , initialise
  , inAppBundle
  , waitForTrust
  , runLoop
  , quit
  , displays
  , scanWindows
  , queryWindow
  , windowFrame
  , windowSpaces
  , focusWindow
  , closeWindow
  , focusNothing
  , setHotkeys
  , setFrame
  , nextFrame
  , framesIdle
  , setHyper
  , showPaused
  ) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8Lenient)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Storable (peek, poke)
import Kineo.Command (Command (..))
import Kineo.Core (Display (..), Pid, SpaceId, WindowInfo (..))
import Kineo.Geometry (Rect (..))
import Kineo.Platform.FFI
import Kineo.Strip (WindowId)

-- | Notifications from macOS, as delivered on the main thread.
data RawEvent
  = RawWindowCreated WindowId
  | RawWindowDestroyed WindowId
  | RawWindowFocused WindowId
  | RawWindowMoved WindowId
  | RawWindowResized WindowId
  | RawWindowMinimized WindowId Bool
  | RawAppTerminated Pid
  | RawAppHidden Pid Bool
  | RawSpaceChanged
  | RawDisplaysChanged
  | RawHotkey Int
  | -- | A click on the menu bar icon, or an item of its menu.
    RawMenu Command
  deriving stock (Eq, Show)

decode :: Int32 -> Int32 -> WindowId -> Maybe RawEvent
decode kind pid arg = case kind of
  1 -> Just (RawWindowCreated arg)
  2 -> Just (RawWindowDestroyed arg)
  3 -> Just (RawWindowFocused arg)
  4 -> Just (RawWindowMoved arg)
  5 -> Just (RawWindowResized arg)
  6 -> Just (RawWindowMinimized arg True)
  7 -> Just (RawWindowMinimized arg False)
  8 -> Just (RawAppTerminated pid)
  9 -> Just (RawAppHidden pid True)
  10 -> Just (RawAppHidden pid False)
  11 -> Just RawSpaceChanged
  12 -> Just RawDisplaysChanged
  13 -> Just (RawHotkey (fromIntegral arg))
  14 | arg == 0 -> Just (RawMenu ReloadConfig)
     | arg == 1 -> Just (RawMenu Quit)
     | arg == 2 -> Just (RawMenu TogglePause)
  _ -> Nothing

-- | Is Kineo allowed to use the Accessibility API? With @prompt@, macOS
-- shows its permission dialog if not.
accessibilityTrusted :: Bool -> IO Bool
accessibilityTrusted prompt = (/= 0) <$> kn_ax_trusted (if prompt then 1 else 0)

-- | Wait up to this many seconds for Accessibility access, keeping the menu
-- bar icon working. Main thread, before 'runLoop'. True once granted.
waitForTrust :: Double -> IO Bool
waitForTrust secs = (/= 0) <$> kn_wait_ax_trusted (realToFrac secs)

-- | Running as Kineo.app, rather than a bare binary from a terminal?
inAppBundle :: IO Bool
inAppBundle = (/= 0) <$> kn_in_app_bundle

-- | Must run on the main thread before anything else.
initialise :: IO ()
initialise = kn_init

-- | Start watching the system and run the Cocoa event loop. Must run on the
-- main thread; never returns. The handler runs on the main thread and must
-- be quick.
runLoop :: (RawEvent -> IO ()) -> IO ()
runLoop handler = do
  fn <- wrapEventFn $ \kind pid arg -> mapM_ handler (decode kind pid arg)
  kn_run fn

quit :: Int -> IO ()
quit = kn_quit . fromIntegral

displays :: IO [Display]
displays = allocaArray maxDisplays $ \buf -> do
  n <- kn_displays buf (fromIntegral maxDisplays)
  map convert <$> peekArray (fromIntegral n) buf
  where
    maxDisplays = 32
    convert d =
      Display
        { displayId = d.displayId
        , frame = rect d.frame
        , visibleFrame = rect d.visible
        , currentSpace = d.space
        , userSpace = d.userSpace /= 0
        }

-- | Every window of every regular app, without subscribing to anything.
-- For diagnostics.
scanWindows :: IO [WindowId]
scanWindows = allocaArray maxWindows $ \buf -> do
  n <- kn_scan_windows buf (fromIntegral maxWindows)
  peekArray (fromIntegral n) buf
  where
    maxWindows = 1024

queryWindow :: WindowId -> IO (Maybe WindowInfo)
queryWindow wid = alloca $ \p -> do
  ok <- kn_query_window wid p
  if ok == 0
    then pure Nothing
    else do
      i <- peek p
      bundle <- decodeUtf8Lenient <$> BS.packCString i.bundleId
      title <- decodeUtf8Lenient <$> BS.packCString i.title
      pure . Just $
        WindowInfo
          { wid = wid
          , pid = i.pid
          , bundleId = bundle
          , title = title
          , space = i.space
          , bounds = rect i.frame
          , standard = i.standard /= 0
          , resizable = i.resizable /= 0
          , movable = i.movable /= 0
          , minimized = i.minimized /= 0
          , fullscreen = i.fullscreen /= 0
          }

windowFrame :: WindowId -> IO (Maybe Rect)
windowFrame wid = alloca $ \p -> do
  ok <- kn_window_frame wid p
  if ok == 0 then pure Nothing else Just . rect <$> peek p

windowSpaces :: [WindowId] -> IO [SpaceId]
windowSpaces wids = withArrayLen wids $ \n ws -> allocaArray n $ \out -> do
  kn_window_spaces ws out (fromIntegral n)
  peekArray n out

focusWindow :: WindowId -> IO ()
focusWindow = kn_window_focus

-- | Close a window as its close button would, so apps can still ask about
-- unsaved changes.
closeWindow :: WindowId -> IO ()
closeWindow = kn_window_close

-- | Leave no window with keyboard focus.
focusNothing :: IO ()
focusNothing = kn_focus_nothing

-- | Register global hotkeys as @(key code, Carbon modifier mask)@; a press
-- arrives as 'RawHotkey' with the index into this list. Replaces any
-- previous set.
setHotkeys :: [(Word, Word)] -> IO ()
setHotkeys keys =
  withArrayLen [CHotkey (fromIntegral k) (fromIntegral m) | (k, m) <- keys] $ \n p ->
    kn_set_hotkeys p (fromIntegral n)

data SetResult = SetOk | UnknownWindow | DeadWindow | SetFailed
  deriving stock (Eq, Show)

-- | Move and/or resize a window.
setFrame :: WindowId -> Rect -> Bool -> Bool -> IO SetResult
setFrame wid (Rect x y w h) position size = alloca $ \p -> do
  poke p (CRect x y w h)
  res <- kn_window_set_frame wid p ((if position then 1 else 0) + (if size then 2 else 0))
  pure $ case res of
    0 -> SetOk
    1 -> UnknownWindow
    2 -> DeadWindow
    _ -> SetFailed

rect :: CRect -> Rect
rect (CRect x y w h) = Rect x y w h

-- | Wait for the display to start on its next frame; returns how many
-- seconds from now that frame will be on screen. 'Nothing' if there is no
-- display to follow.
nextFrame :: IO (Maybe Double)
nextFrame = (\t -> if t < 0 then Nothing else Just (realToFrac t)) <$> kn_next_frame

-- | Nothing is animating: stop listening for display frames.
framesIdle :: IO ()
framesIdle = kn_frames_idle

-- | Caps Lock as hyper: on or off, and whether a tap alone sends Escape.
-- False if it could not be turned on.
-- | Dim the menu bar icon while paused.
showPaused :: Bool -> IO ()
showPaused p = kn_set_paused (if p then 1 else 0)

setHyper :: Bool -> Bool -> IO Bool
setHyper on escape = (/= 0) <$> kh_configure (fromBool on) (fromBool escape)
  where
    fromBool b = if b then 1 else 0
