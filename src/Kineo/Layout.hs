-- | Turning a strip into screen rectangles.
--
-- Columns are laid out in "strip coordinates": column 0 starts at 0 and
-- every column is followed by one gap. A space's /scroll/ is the strip
-- coordinate that lines up with the left edge of the usable area, so
-- scrolling never touches the strip itself.
module Kineo.Layout
  ( Params (..)
  , FocusMode (..)
  , Placement (..)
  , usableWidth
  , columnPx
  , fractionFor
  , spans
  , scrollFor
  , place
  , stow
  , unpark
  ) where

import Data.List.NonEmpty qualified as NE
import Kineo.Geometry (Rect (..), bottom, centerX, intersects, right)
import Kineo.Strip (Column (..), Strip, WindowId, columns)

data FocusMode
  = -- | Scroll as little as possible to bring the focused column into view.
    Reveal
  | -- | Keep the focused column centred where the strip allows it.
    Center
  deriving stock (Eq, Show)

data Params = Params
  { gap :: Double
  -- ^ Space between columns and between windows stacked in a column.
  , margin :: Double
  -- ^ Space between windows and the edges of the display.
  , sliver :: Double
  -- ^ How much of an off-screen window stays visible at the display edge.
  }
  deriving stock (Eq, Show)

data Placement = Placement
  { window :: WindowId
  , rect :: Rect
  , onScreen :: Bool
  -- ^ False when the window has been parked at the edge of the display.
  }
  deriving stock (Eq, Show)

usableWidth :: Params -> Rect -> Double
usableWidth p visible = visible.w - 2 * p.margin

-- | Pixel width of a column. Chosen so that columns whose fractions add up
-- to 1 exactly fill the usable width, gaps included.
columnPx :: Params -> Double -> Double -> Double
columnPx p usable frac = max 1 (frac * (usable + p.gap) - p.gap)

-- | Inverse of 'columnPx'.
fractionFor :: Params -> Double -> Double -> Double
fractionFor p usable px = (px + p.gap) / (usable + p.gap)

-- | Strip-coordinate @(x, width)@ of each column.
spans :: Params -> Double -> [Double] -> [(Double, Double)]
spans p usable = go 0
  where
    go _ [] = []
    go sx (f : fs) = let cw = columnPx p usable f in (sx, cw) : go (sx + cw + p.gap) fs

-- | The scroll that shows the focused column, starting from the previous
-- scroll. The result never scrolls past either end of the strip.
scrollFor :: Params -> FocusMode -> Double -> [Double] -> Maybe Int -> Double -> Double
scrollFor p mode usable fracs focus prev = clamp wanted
  where
    ss = spans p usable fracs
    total = case reverse ss of
      ((sx, cw) : _) -> sx + cw
      [] -> 0
    clamp v = max 0 (min (max 0 (total - usable)) v)
    wanted = case focus of
      Just i | i >= 0, i < length ss -> onFocus (ss !! i)
      _ -> prev
    onFocus (cx, cw) = case mode of
      Center -> cx + cw / 2 - usable / 2
      Reveal
        | cw >= usable || cx < prev -> cx
        | cx + cw > prev + usable -> cx + cw - usable
        | otherwise -> prev

-- | Screen rectangles for every window of a strip shown on a display.
--
-- @full@ is the whole display, @visible@ the part not covered by the menu
-- bar and Dock, and @others@ the frames of every other display. A window that
-- is wholly off the display, or that would spill onto a neighbouring
-- display, is parked at the edge with a 'sliver' left showing: macOS will
-- not let a window leave the screen entirely, and with separate Spaces a
-- window that overlaps another display more than its own jumps to it.
place :: Params -> Rect -> Rect -> [Rect] -> Double -> Strip -> [Placement]
place p full visible others scroll strip = map settle (rects p visible scroll strip)
  where
    settle (wid, r)
      | r.x >= full.x - 1 && right r <= right full + 1 = Placement wid r True
      -- Partly visible is fine, but only if it reaches past the margin into
      -- the usable area; a few pixels in the margin is just clutter.
      | shownWidth r > p.margin + p.sliver && not (any (intersects r) others) = Placement wid r True
      | otherwise = Placement wid (park p full others False r) False

    shownWidth r = min (right r) (right full) - max r.x full.x

-- | Placements for a strip that is not being shown: the windows of a
-- workspace other than the active one. Every window is parked below the
-- display, under where it would otherwise be, so switching workspaces slides
-- it up into place.
stow :: Params -> Rect -> Rect -> [Rect] -> Double -> Strip -> [Placement]
stow p full visible others scroll strip =
  [Placement wid (park p full others True r) False | (wid, r) <- rects p visible scroll strip]

-- | Where each window of a strip goes, before parking.
rects :: Params -> Rect -> Double -> Strip -> [(WindowId, Rect)]
rects p visible scroll strip = concat (zipWith placeColumn cols (spans p usable (map (.width) cols)))
  where
    cols = columns strip
    usable = usableWidth p visible
    top = visible.y + p.margin
    height = visible.h - 2 * p.margin

    placeColumn c (sx, cw) =
      let ws = NE.toList c.stack
          n = fromIntegral (length ws)
          rh = (height - (n - 1) * p.gap) / n
          x0 = visible.x + p.margin + sx - scroll
       in [(wid, Rect x0 (top + i * (rh + p.gap)) cw rh) | (i, wid) <- zip [0 ..] ws]

-- | Park a window at the edge of a display with a 'sliver' showing: beside
-- it on the nearer side, or along the bottom, whichever is preferred and
-- does not touch another display.
park :: Params -> Rect -> [Rect] -> Bool -> Rect -> Rect
park p full others preferBelow r =
  let beside
        | centerX r < centerX full = r {x = full.x - r.w + p.sliver}
        | otherwise = r {x = right full - p.sliver}
      below = r {x = max full.x (min r.x (right full - r.w)), y = bottom full - p.sliver}
      order = if preferBelow then [below, beside] else [beside, below]
   in case filter (\c -> not (any (intersects c) others)) order of
        (c : _) -> c
        [] -> beside

-- | Pull a rectangle fully inside an area, for handing windows back to the
-- user when the window manager exits.
unpark :: Rect -> Rect -> Rect
unpark area r =
  let wd = min r.w area.w
      ht = min r.h area.h
   in Rect
        { x = max area.x (min r.x (right area - wd))
        , y = max area.y (min r.y (bottom area - ht))
        , w = wd
        , h = ht
        }
