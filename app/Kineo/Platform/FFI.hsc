-- | Raw bindings to cbits/kineo.h. Struct layouts come from the C compiler
-- via hsc2hs, never from hand-counted offsets.
module Kineo.Platform.FFI
  ( CRect (..)
  , CWindowInfo (..)
  , CDisplay (..)
  , CHotkey (..)
  , EventFn
  , wrapEventFn
  , kn_ax_trusted
  , kn_init
  , kn_run
  , kn_quit
  , kn_displays
  , kn_scan_windows
  , kn_query_window
  , kn_window_frame
  , kn_window_spaces
  , kn_window_focus
  , kn_set_hotkeys
  , kn_window_set_frame
  ) where

import Data.Int (Int32)
import Data.Word (Word32, Word64, Word8)
import Foreign.C.String (CString)
import Foreign.C.Types (CBool (..), CInt (..))
import Foreign.Ptr (FunPtr, Ptr, plusPtr)
import Foreign.Storable (Storable (..))

#include "kineo.h"

data CRect = CRect Double Double Double Double

instance Storable CRect where
  sizeOf _ = #{size kn_rect}
  alignment _ = #{alignment kn_rect}
  peek p =
    CRect
      <$> #{peek kn_rect, x} p
      <*> #{peek kn_rect, y} p
      <*> #{peek kn_rect, w} p
      <*> #{peek kn_rect, h} p
  poke p (CRect x y w h) = do
    #{poke kn_rect, x} p x
    #{poke kn_rect, y} p y
    #{poke kn_rect, w} p w
    #{poke kn_rect, h} p h

data CWindowInfo = CWindowInfo
  { pid :: Int32
  , space :: Word64
  , frame :: CRect
  , standard :: Word8
  , resizable :: Word8
  , movable :: Word8
  , minimized :: Word8
  , fullscreen :: Word8
  , bundleId :: CString
  -- ^ Points into the struct: only valid while it is.
  , title :: CString
  }

instance Storable CWindowInfo where
  sizeOf _ = #{size kn_window_info}
  alignment _ = #{alignment kn_window_info}
  peek p =
    CWindowInfo
      <$> #{peek kn_window_info, pid} p
      <*> #{peek kn_window_info, space} p
      <*> #{peek kn_window_info, frame} p
      <*> #{peek kn_window_info, standard} p
      <*> #{peek kn_window_info, resizable} p
      <*> #{peek kn_window_info, movable} p
      <*> #{peek kn_window_info, minimized} p
      <*> #{peek kn_window_info, fullscreen} p
      <*> pure (#{ptr kn_window_info, bundle_id} p)
      <*> pure (#{ptr kn_window_info, title} p)
  poke _ _ = error "CWindowInfo is read-only"

data CDisplay = CDisplay
  { displayId :: Word32
  , frame :: CRect
  , visible :: CRect
  , space :: Word64
  , userSpace :: Word8
  }

instance Storable CDisplay where
  sizeOf _ = #{size kn_display}
  alignment _ = #{alignment kn_display}
  peek p =
    CDisplay
      <$> #{peek kn_display, id} p
      <*> #{peek kn_display, frame} p
      <*> #{peek kn_display, visible} p
      <*> #{peek kn_display, space} p
      <*> #{peek kn_display, user_space} p
  poke _ _ = error "CDisplay is read-only"

data CHotkey = CHotkey Word32 Word32

instance Storable CHotkey where
  sizeOf _ = #{size kn_hotkey}
  alignment _ = #{alignment kn_hotkey}
  peek p = CHotkey <$> #{peek kn_hotkey, keycode} p <*> #{peek kn_hotkey, modifiers} p
  poke p (CHotkey k m) = #{poke kn_hotkey, keycode} p k >> #{poke kn_hotkey, modifiers} p m

type EventFn = Int32 -> Int32 -> Word32 -> IO ()

foreign import ccall "wrapper" wrapEventFn :: EventFn -> IO (FunPtr EventFn)

foreign import ccall safe "kn_ax_trusted" kn_ax_trusted :: CBool -> IO CBool
foreign import ccall safe "kn_init" kn_init :: IO ()
-- 'safe' so the RTS keeps running other Haskell threads while the main
-- thread sits in the Cocoa run loop, and so callbacks can re-enter Haskell.
foreign import ccall safe "kn_run" kn_run :: FunPtr EventFn -> IO ()
foreign import ccall safe "kn_quit" kn_quit :: CInt -> IO ()
foreign import ccall safe "kn_displays" kn_displays :: Ptr CDisplay -> CInt -> IO CInt
foreign import ccall safe "kn_scan_windows" kn_scan_windows :: Ptr Word32 -> CInt -> IO CInt
foreign import ccall safe "kn_query_window" kn_query_window :: Word32 -> Ptr CWindowInfo -> IO CBool
foreign import ccall safe "kn_window_frame" kn_window_frame :: Word32 -> Ptr CRect -> IO CBool
foreign import ccall safe "kn_window_spaces" kn_window_spaces :: Ptr Word32 -> Ptr Word64 -> CInt -> IO ()
foreign import ccall safe "kn_window_focus" kn_window_focus :: Word32 -> IO ()
foreign import ccall safe "kn_set_hotkeys" kn_set_hotkeys :: Ptr CHotkey -> CInt -> IO ()
foreign import ccall safe "kn_window_set_frame" kn_window_set_frame :: Word32 -> Ptr CRect -> Int32 -> IO CInt
