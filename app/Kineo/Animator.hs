-- | Moves windows to where the layout wants them, smoothly.
--
-- The layout hands over complete target sets; the animator works out which
-- windows actually need to move and slides them there on its own thread.
-- Resizing an app's window every frame is slow and makes content jitter,
-- so a window takes its final size on the first frame and only its
-- position is animated.
--
-- Frames follow the display's refresh. Each window has its own sender
-- thread that makes the (blocking) accessibility calls, and only ever
-- has the latest frame to send: an app that is slow to answer skips
-- frames of its own windows and holds up no one else's.
module Kineo.Animator
  ( Animator
  , start
  , setConfig
  , setTargets
  , placeNow
  , forget
  , forgetAll
  , observed
  , lastSet
  , busy
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import Control.Monad (forM, forM_, void, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import Kineo.Config (Animation (..))
import Kineo.Geometry (Rect (..), approxEq)
import Kineo.Layout (Placement (..))
import Kineo.Motion (Motion (..))
import Kineo.Motion qualified as Motion
import Kineo.Platform (SetResult (..), framesIdle, nextFrame, setFrame, windowFrame)
import Kineo.Strip (WindowId)

-- | A window on its way.
data Flight = Flight
  { motion :: Motion
  , begun :: Bool
  -- ^ Has its first frame been sent?
  , resize :: Bool
  -- ^ Does the window need resizing, going by its last known frame? Not
  -- the motion's start: a window shuffled about off screen starts at its
  -- target.
  }

data State = State
  { current :: Map WindowId Rect
  -- ^ The frame each window has now, as far as we know.
  , flights :: Map WindowId Flight
  , touched :: Map WindowId Double
  -- ^ When we last moved each window.
  , shown :: Map WindowId Bool
  -- ^ Whether each window's last target was on screen.
  }

emptyState :: State
emptyState = State Map.empty Map.empty Map.empty Map.empty

-- | A frame for a window's sender.
data Send = Send
  { rect :: Rect
  , sizeIt :: Bool
  -- ^ Set the size too, not just the position.
  , final :: Bool
  -- ^ The last frame of its motion.
  }

data Mailbox = Idle | Pending Send | Stopped

data Animator = Animator
  { state :: TVar State
  , config :: TVar Animation
  , senders :: TVar (Map WindowId (TVar Mailbox))
  , dead :: WindowId -> IO ()
  , tooWide :: WindowId -> Double -> IO ()
  }

-- | Start the animation thread. @dead@ is told about windows that turned
-- out to be gone when we tried to move them, and @tooWide@ about windows
-- whose app kept them wider than asked, with the width they kept.
start :: Animation -> (WindowId -> IO ()) -> (WindowId -> Double -> IO ()) -> IO Animator
start cfg dead tooWide = do
  a <-
    Animator
      <$> newTVarIO emptyState
      <*> newTVarIO cfg
      <*> newTVarIO Map.empty
      <*> pure dead
      <*> pure tooWide
  _ <- forkIO (run a)
  pure a

setConfig :: Animator -> Animation -> IO ()
setConfig a = atomically . writeTVar a.config

-- | Animate every window to its placement. Windows already there, or
-- already heading there, are left alone.
setTargets :: Animator -> [Placement] -> IO ()
setTargets a ps = do
  st0 <- readTVarIO a.state
  -- Windows we have never moved: ask where they are, outside the transaction.
  found <- forM [p.window | p <- ps, not (Map.member p.window st0.current)] $ \wid ->
    (wid,) <$> windowFrame wid
  now <- getMonotonicTime
  cfg <- readTVarIO a.config
  atomically . modifyTVar' a.state $ \st ->
    let st' = st {current = Map.union st.current (Map.mapMaybe id (Map.fromList found))}
     in foldl (retarget cfg now) st' ps

retarget :: Animation -> Double -> State -> Placement -> State
retarget cfg now st p = case Map.lookup p.window st.flights of
  Just f | approxEq f.motion.to p.rect -> st'
  -- Already moving: carry on from where it has got to, at its speed.
  Just f | f.begun -> fly (Motion.redirect cfg now p.rect f.motion)
  Just f -> fly (Motion.begin now f.motion.from p.rect)
  Nothing -> case Map.lookup p.window st.current of
    Just cur | approxEq cur p.rect -> st'
    Just cur -> fly (Motion.begin now cur p.rect)
    Nothing -> fly (Motion.begin now p.rect p.rect)
  where
    wasShown = Map.findWithDefault True p.window st.shown
    st' = st {shown = Map.insert p.window p.onScreen st.shown}
    fly m =
      -- Shuffling windows around behind the screen edge needn't be animated.
      let m' = if wasShown || p.onScreen then m else Motion.begin now p.rect p.rect
          resize = maybe True (not . sameSize p.rect) (Map.lookup p.window st.current)
       in st' {flights = Map.insert p.window (Flight m' False resize) st.flights}

-- | Animate frame after frame, in step with the display, and rest while
-- nothing is moving.
run :: Animator -> IO ()
run a = go 0
  where
    go lastShown = do
      idle <- Map.null . (.flights) <$> readTVarIO a.state
      when idle $ do
        framesIdle
        atomically (readTVar a.state >>= \st -> when (Map.null st.flights) retry)
      cfg <- readTVarIO a.config
      shown <- nextShown (1 / fromIntegral (max 1 cfg.fps)) lastShown
      frame a cfg shown
      go shown

-- | Wait for the display's next frame, at most @fps@ times a second, and
-- say when it will be on screen. Without a display link, just sleep.
nextShown :: Double -> Double -> IO Double
nextShown interval lastShown =
  nextFrame >>= \case
    Just ahead -> do
      shown <- (+ ahead) <$> getMonotonicTime
      if shown - lastShown >= 0.9 * interval then pure shown else nextShown interval lastShown
    Nothing -> do
      now <- getMonotonicTime
      let wake = lastShown + interval
      when (wake > now) $ threadDelay (round ((wake - now) * 1e6))
      getMonotonicTime

-- | One animation frame: where every moving window should be at @shown@,
-- when the display will show it, handed to the windows' senders.
--
-- Every call into an app costs it work, so a frame only sends what
-- changed: most motions only move a window, and resizing one makes the
-- app lay out and redraw it; near the end of a motion several frames
-- round to the same pixel.
frame :: Animator -> Animation -> Double -> IO ()
frame a cfg shown = do
  st <- readTVarIO a.state
  now <- getMonotonicTime
  let interval = 1 / fromIntegral (max 1 cfg.fps)
      plan (wid, f) =
        let
          -- A motion starts on its first frame, however long it waited
          -- for it, so that frame doesn't jump ahead.
          f' = if f.begun then f else f {motion = f.motion {started = shown - interval}, begun = True}
          r = Motion.at cfg shown f'.motion
          done = Motion.finished cfg shown f'.motion
          sizeIt = f.resize && (done || not f.begun)
          moved = maybe True (not . samePlace r) (Map.lookup wid st.current)
         in
          (wid, f', r, done, [Send r sizeIt (done && f.resize) | sizeIt || moved])
      plans = map plan (Map.toList st.flights)
  forM_ plans $ \(wid, _, _, _, sends) -> mapM_ (post a wid) sends
  let update s (wid, f, r, done, sends) =
        let sent = not (null sends)
            s'
              | sent =
                  s
                    { current = Map.insert wid r s.current
                    , touched = Map.insert wid now s.touched
                    }
              | otherwise = s
         in case Map.lookup wid s.flights of
              -- Retargeted since: keep the new motion.
              Just f' | not (approxEq f'.motion.to f.motion.to) -> s'
              _ -> s' {flights = if done then Map.delete wid s.flights else Map.insert wid f s.flights}
  atomically . modifyTVar' a.state $ \s -> foldl update s plans

-- | Hand a frame to a window's sender, replacing any it hasn't sent yet.
post :: Animator -> WindowId -> Send -> IO ()
post a wid s = do
  box <-
    readTVarIO a.senders >>= \m -> case Map.lookup wid m of
      Just box -> pure box
      Nothing -> do
        box <- newTVarIO Idle
        atomically $ modifyTVar' a.senders (Map.insert wid box)
        _ <- forkIO (sender a wid box)
        pure box
  atomically . modifyTVar' box $ \case
    Stopped -> Stopped
    Idle -> Pending s
    -- A size not sent yet is still owed.
    Pending old -> Pending s {sizeIt = s.sizeIt || old.sizeIt}

-- | Send one window its frames, as fast as its app takes them.
sender :: Animator -> WindowId -> TVar Mailbox -> IO ()
sender a wid box = loop
  where
    loop = do
      next <-
        atomically $
          readTVar box >>= \case
            Idle -> retry
            Stopped -> pure Nothing
            Pending s -> Just s <$ writeTVar box Idle
      forM_ next $ \s -> do
        res <- setFrame wid s.rect True s.sizeIt
        -- After a final resize, position again: an app may have nudged the
        -- window while it resized.
        when (s.final && s.sizeIt && res == SetOk) $ void (setFrame wid s.rect True False)
        now <- getMonotonicTime
        atomically . modifyTVar' a.state $ \st ->
          if Map.member wid st.current then st {touched = Map.insert wid now st.touched} else st
        when (res == DeadWindow) (a.dead wid)
        when (s.final && s.sizeIt && res == SetOk) (checkWidth a wid s.rect)
        loop

-- | Within half a pixel, as the app will round it.
sameSize, samePlace :: Rect -> Rect -> Bool
sameSize a b = abs (a.w - b.w) < 0.5 && abs (a.h - b.h) < 0.5
samePlace a b = round a.x == (round b.x :: Int) && round a.y == (round b.y :: Int)

-- | Apps can refuse to shrink a window below their minimum size. Look a
-- moment after it was given its final size (some apps resize
-- asynchronously), and only if nothing has moved it since.
checkWidth :: Animator -> WindowId -> Rect -> IO ()
checkWidth a wid r = void . forkIO $ do
  threadDelay 250000
  st <- readTVarIO a.state
  let settled = not (Map.member wid st.flights) && fmap (.w) (Map.lookup wid st.current) == Just r.w
  when settled $
    windowFrame wid >>= \case
      Just actual | actual.w > r.w + 2 -> a.tooWide wid actual.w
      _ -> pure ()

-- | Put windows straight into place, with no animation. For shutting down.
placeNow :: [(WindowId, Rect)] -> IO ()
placeNow rs = forM_ rs $ \(wid, r) -> setFrame wid r True True

forget :: Animator -> WindowId -> IO ()
forget a wid = atomically $ do
  modifyTVar' a.state $ \st ->
    st
      { current = Map.delete wid st.current
      , flights = Map.delete wid st.flights
      , touched = Map.delete wid st.touched
      , shown = Map.delete wid st.shown
      }
  boxes <- readTVar a.senders
  forM_ (Map.lookup wid boxes) (`writeTVar` Stopped)
  writeTVar a.senders (Map.delete wid boxes)

forgetAll :: Animator -> IO ()
forgetAll a = atomically $ do
  writeTVar a.state emptyState
  readTVar a.senders >>= mapM_ (`writeTVar` Stopped)
  writeTVar a.senders Map.empty

-- | Record where a window really is after someone else moved it, so the
-- next layout puts it back.
observed :: Animator -> WindowId -> Rect -> IO ()
observed a wid r = atomically . modifyTVar' a.state $ \st -> st {current = Map.insert wid r st.current}

lastSet :: Animator -> WindowId -> IO (Maybe Rect)
lastSet a wid = Map.lookup wid . (.current) <$> readTVarIO a.state

-- | Is this window moving, or did we move it a moment ago? Its move and
-- resize notifications are then echoes of our own work.
busy :: Animator -> WindowId -> IO Bool
busy a wid = do
  st <- readTVarIO a.state
  now <- getMonotonicTime
  pure $
    Map.member wid st.flights
      || fromMaybe False ((\t -> now - t < 0.3) <$> Map.lookup wid st.touched)
