-- | Plain rectangle geometry in macOS global screen coordinates: the origin
-- is the top-left of the primary display and y grows downwards (the same
-- convention CoreGraphics and the Accessibility API use).
module Kineo.Geometry
  ( Rect (..)
  , right
  , bottom
  , centerX
  , centerY
  , contains
  , intersects
  , approxEq
  , lerp
  ) where

data Rect = Rect {x :: Double, y :: Double, w :: Double, h :: Double}
  deriving stock (Eq, Show)

right, bottom, centerX, centerY :: Rect -> Double
right r = r.x + r.w
bottom r = r.y + r.h
centerX r = r.x + r.w / 2
centerY r = r.y + r.h / 2

-- | Does the rectangle contain the point?
contains :: Rect -> (Double, Double) -> Bool
contains r (px, py) = px >= r.x && px < right r && py >= r.y && py < bottom r

-- | Do the rectangles overlap by a positive area?
intersects :: Rect -> Rect -> Bool
intersects a b = a.x < right b && b.x < right a && a.y < bottom b && b.y < bottom a

-- | Equal to within half a pixel on every edge; window servers round frames.
approxEq :: Rect -> Rect -> Bool
approxEq a b = all (< 0.5) [abs (a.x - b.x), abs (a.y - b.y), abs (a.w - b.w), abs (a.h - b.h)]

-- | Linear interpolation, @t@ in @[0, 1]@.
lerp :: Rect -> Rect -> Double -> Rect
lerp a b t =
  Rect
    { x = a.x + (b.x - a.x) * t
    , y = a.y + (b.y - a.y) * t
    , w = a.w + (b.w - a.w) * t
    , h = a.h + (b.h - a.h) * t
    }
