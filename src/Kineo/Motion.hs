-- | How a window travels to its target over time.
--
-- Positions move along a curve; sizes jump straight to the target (an app
-- resizing every frame is slow and its content jitters). A motion can be
-- redirected to a new target part way: a spring carries its speed into
-- the new motion, so repeated commands flow into each other instead of
-- starting from a standstill each time. The eased curves always start
-- from rest.
module Kineo.Motion
  ( Motion (..)
  , begin
  , at
  , velocity
  , finished
  , redirect
  ) where

import Kineo.Config (Animation (..), Easing (..))
import Kineo.Geometry (Rect (..))

data Motion = Motion
  { from :: Rect
  -- ^ Where the window was at 'started'.
  , speed :: (Double, Double)
  -- ^ Its velocity then, in pixels per second.
  , to :: Rect
  , started :: Double
  }
  deriving stock (Eq, Show)

-- | A motion from rest.
begin :: Double -> Rect -> Rect -> Motion
begin now from to = Motion {from, speed = (0, 0), to, started = now}

duration :: Animation -> Double
duration cfg = fromIntegral cfg.durationMs / 1000

-- | The spring's natural frequency. Critically damped, it has covered all
-- but 0.3% of the way from rest after the configured duration.
omega :: Animation -> Double
omega cfg = 8 / duration cfg

-- | Where the window is at a given time.
at :: Animation -> Double -> Motion -> Rect
at cfg now m
  | cfg.durationMs <= 0 = m.to
  | otherwise = case cfg.easing of
      Spring ->
        let (x, _) = spring (omega cfg) t (m.from.x - m.to.x) (fst m.speed)
            (y, _) = spring (omega cfg) t (m.from.y - m.to.y) (snd m.speed)
         in m.to {x = m.to.x + x, y = m.to.y + y}
      e ->
        let k = ease e (min 1 (t / duration cfg))
         in m.to {x = m.from.x + (m.to.x - m.from.x) * k, y = m.from.y + (m.to.y - m.from.y) * k}
  where
    t = max 0 (now - m.started)

-- | How fast the window is moving at a given time, in pixels per second.
velocity :: Animation -> Double -> Motion -> (Double, Double)
velocity cfg now m
  | cfg.durationMs <= 0 = (0, 0)
  | otherwise = case cfg.easing of
      Spring ->
        ( snd (spring (omega cfg) t (m.from.x - m.to.x) (fst m.speed))
        , snd (spring (omega cfg) t (m.from.y - m.to.y) (snd m.speed))
        )
      _ -> (0, 0)
  where
    t = max 0 (now - m.started)

-- | Has the window arrived? Springs have no fixed end: they are done once
-- within half a pixel and moving under a quarter pixel a frame at 120 fps.
finished :: Animation -> Double -> Motion -> Bool
finished cfg now m
  | cfg.durationMs <= 0 = True
  | otherwise = case cfg.easing of
      Spring ->
        let r = at cfg now m
            (vx, vy) = velocity cfg now m
         in abs (r.x - m.to.x) < 0.5 && abs (r.y - m.to.y) < 0.5 && abs vx < 30 && abs vy < 30
      _ -> now - m.started >= duration cfg

-- | Head for a new target from wherever the motion has got to.
redirect :: Animation -> Double -> Rect -> Motion -> Motion
redirect cfg now to m =
  Motion {from = at cfg now m, speed = velocity cfg now m, to, started = now}

-- | A critically damped spring: displacement and velocity after @t@
-- seconds, from displacement @d0@ and velocity @v0@.
spring :: Double -> Double -> Double -> Double -> (Double, Double)
spring w t d0 v0 =
  let b = v0 + w * d0
      decay = exp (-w * t)
   in ((d0 + b * t) * decay, (v0 - w * b * t) * decay)

-- | Eased progress for a fraction @t@ of the duration.
ease :: Easing -> Double -> Double
ease e t = case e of
  Linear -> t
  EaseOut -> 1 - (1 - t) ^ (3 :: Int)
  EaseInOut
    | t < 0.5 -> 4 * t * t * t
    | otherwise -> 1 - ((-2 * t + 2) ^ (3 :: Int)) / 2
  Spring -> let (d, _) = spring 8 t (-1) 0 in 1 + d
