-- | Moves windows to where the layout wants them, smoothly.
--
-- The layout hands over complete target sets; the animator works out which
-- windows actually need to move and slides them there on its own thread.
-- Resizing an app's window every frame is slow and makes content jitter,
-- so a window takes its final size on the first frame and only its
-- position is animated.
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
import Control.Monad (forM, forM_, forever, unless, when)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import GHC.Clock (getMonotonicTime)
import Kineo.Config (Animation (..), Easing (..))
import Kineo.Geometry (Rect (..), approxEq, lerp)
import Kineo.Layout (Placement (..))
import Kineo.Platform (SetResult (..), setFrame, windowFrame)
import Kineo.Strip (WindowId)

data Motion = Motion
  { from :: Rect
  , to :: Rect
  , started :: Double
  , sized :: Bool
  -- ^ Has the final size been applied yet?
  }

data State = State
  { current :: Map WindowId Rect
  -- ^ The frame each window has now, as far as we know.
  , motions :: Map WindowId Motion
  , touched :: Map WindowId Double
  -- ^ When we last moved each window.
  , shown :: Map WindowId Bool
  -- ^ Whether each window's last target was on screen.
  }

data Animator = Animator
  { state :: TVar State
  , config :: TVar Animation
  }

-- | Start the animation thread. @dead@ is told about windows that turned
-- out to be gone when we tried to move them.
start :: Animation -> (WindowId -> IO ()) -> IO Animator
start cfg dead = do
  a <- Animator <$> newTVarIO (State Map.empty Map.empty Map.empty Map.empty) <*> newTVarIO cfg
  _ <- forkIO (forever (frame a dead))
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
retarget cfg now st p = case Map.lookup p.window st.motions of
  Just m | approxEq m.to p.rect -> st'
  Just m -> begin (position cfg now m)
  Nothing -> case Map.lookup p.window st.current of
    Just cur | approxEq cur p.rect -> st'
    Just cur -> begin cur
    Nothing -> begin p.rect
  where
    wasShown = Map.findWithDefault True p.window st.shown
    st' = st {shown = Map.insert p.window p.onScreen st.shown}
    begin cur =
      -- Shuffling windows around behind the screen edge needn't be animated.
      let from = if wasShown || p.onScreen then cur else p.rect
       in st' {motions = Map.insert p.window (Motion from p.rect now False) st.motions}

-- | Where a motion has got to at a given time.
position :: Animation -> Double -> Motion -> Rect
position cfg now m = (lerp m.from m.to (ease cfg.easing (progress cfg now m))) {w = m.to.w, h = m.to.h}

progress :: Animation -> Double -> Motion -> Double
progress cfg now m
  | cfg.durationMs <= 0 = 1
  | otherwise = max 0 (min 1 ((now - m.started) / (fromIntegral cfg.durationMs / 1000)))

ease :: Easing -> Double -> Double
ease e t = case e of
  Linear -> t
  EaseOut -> 1 - (1 - t) ^ (3 :: Int)
  EaseInOut
    | t < 0.5 -> 4 * t * t * t
    | otherwise -> 1 - ((-2 * t + 2) ^ (3 :: Int)) / 2

-- | One animation frame. Blocks while nothing is moving.
frame :: Animator -> (WindowId -> IO ()) -> IO ()
frame a dead = do
  ms <- atomically $ do
    st <- readTVar a.state
    when (Map.null st.motions) retry
    pure st.motions
  cfg <- readTVarIO a.config
  now <- getMonotonicTime
  applied <- forM (Map.toList ms) $ \(wid, m) -> do
    let done = progress cfg now m >= 1
        r = position cfg now m
    res <-
      if done
        then do
          -- Size, then position again: an app may have nudged the window
          -- while it resized.
          _ <- setFrame wid r True True
          setFrame wid r True False
        else setFrame wid r True (not m.sized)
    when (res == DeadWindow) (dead wid)
    pure (wid, m, r, done)
  after <- getMonotonicTime
  atomically . modifyTVar' a.state $ \st ->
    foldl
      ( \s (wid, m, r, done) ->
          case Map.lookup wid s.motions of
            -- Retargeted while we were moving it: keep the new motion.
            Just m' | not (approxEq m'.to m.to) -> s {current = Map.insert wid r s.current}
            _ ->
              s
                { current = Map.insert wid r s.current
                , touched = Map.insert wid after s.touched
                , motions =
                    if done
                      then Map.delete wid s.motions
                      else Map.insert wid m {sized = True} s.motions
                }
      )
      st
      applied
  let budget = 1 / fromIntegral (max 1 cfg.fps)
      spent = after - now
  unless (spent >= budget) $ threadDelay (round ((budget - spent) * 1e6))

-- | Put windows straight into place, with no animation. For shutting down.
placeNow :: [(WindowId, Rect)] -> IO ()
placeNow rs = forM_ rs $ \(wid, r) -> setFrame wid r True True

forget :: Animator -> WindowId -> IO ()
forget a wid = atomically . modifyTVar' a.state $ \st ->
  st
    { current = Map.delete wid st.current
    , motions = Map.delete wid st.motions
    , touched = Map.delete wid st.touched
    , shown = Map.delete wid st.shown
    }

forgetAll :: Animator -> IO ()
forgetAll a = atomically (writeTVar a.state (State Map.empty Map.empty Map.empty Map.empty))

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
    Map.member wid st.motions
      || fromMaybe False ((\t -> now - t < 0.3) <$> Map.lookup wid st.touched)
