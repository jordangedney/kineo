-- | A typed, Haskell-shaped view of the macOS layer in cbits/.
module Kineo.Platform
  ( RawEvent (..)
  , SetResult (..)
  , accessibilityTrusted
  , initialise
  , runLoop
  , quit
  , displays
  , scanWindows
  , queryWindow
  , windowFrame
  , windowSpaces
  , focusWindow
  , setHotkeys
  , setFrame
  ) where

import Data.ByteString qualified as BS
import Data.Int (Int32)
import Data.Text.Encoding (decodeUtf8Lenient)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, peekArray, withArrayLen)
import Foreign.Storable (peek, poke)
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
  _ -> Nothing

-- | Is Kineo allowed to use the Accessibility API? With @prompt@, macOS
-- shows its permission dialog if not.
accessibilityTrusted :: Bool -> IO Bool
accessibilityTrusted prompt = (/= 0) <$> kn_ax_trusted (if prompt then 1 else 0)

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
