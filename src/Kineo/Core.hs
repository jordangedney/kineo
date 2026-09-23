-- | The window manager as a pure state machine.
--
-- The platform layer turns macOS notifications into 'Event's, 'step' folds
-- them into the 'World' and answers with 'Effect's, and the platform layer
-- carries those out. Nothing in here performs IO, so every behaviour can be
-- tested by feeding in events and looking at what comes out.
module Kineo.Core
  ( -- * State
    World (..)
  , Tracked (..)
  , Space (..)
  , Display (..)
  , WindowInfo (..)
  , SpaceId
  , Pid
  , emptyWorld
  , emptySpace

    -- * Transitions
  , Event (..)
  , Effect (..)
  , step
  , tileable
  , isVisible
  , visibleStrip
  , layoutAll
  , released
  ) where

import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Kineo.Command (Command (..))
import Kineo.Config (Config (..), Rule (..), ruleFor)
import Kineo.Geometry (Rect (..), centerX, centerY, contains)
import Kineo.Layout (FocusMode (..), Placement (..), fractionFor, place, scrollFor, unpark, usableWidth)
import Kineo.Strip (Column (..), Dir (..), Strip, WindowId)
import Kineo.Strip qualified as Strip

type SpaceId = Word64

type Pid = Int32

data Display = Display
  { displayId :: Word32
  , frame :: Rect
  , visibleFrame :: Rect
  -- ^ The frame minus the menu bar and Dock.
  , currentSpace :: SpaceId
  , userSpace :: Bool
  -- ^ False for native full-screen spaces, which Kineo leaves alone.
  }
  deriving stock (Eq, Show)

-- | What the platform knows about a window when it first appears.
data WindowInfo = WindowInfo
  { wid :: WindowId
  , pid :: Pid
  , bundleId :: Text
  , title :: Text
  , space :: SpaceId
  -- ^ 0 when unknown.
  , bounds :: Rect
  , standard :: Bool
  -- ^ An ordinary document window, not a dialog, sheet, panel or popover.
  , resizable :: Bool
  , movable :: Bool
  , minimized :: Bool
  , fullscreen :: Bool
  }
  deriving stock (Eq, Show)

-- | A window Kineo manages. Unless it is floating it sits in exactly one
-- strip: the one belonging to its space.
data Tracked = Tracked
  { owner :: Pid
  , onSpace :: SpaceId
  , isMinimized :: Bool
  , isFloating :: Bool
  , originX :: Double
  -- ^ Where the window was when discovered, to keep startup order natural.
  }
  deriving stock (Eq, Show)

data Space = Space
  { strip :: Strip
  , scroll :: Double
  -- ^ Strip coordinate at the left edge of the usable area.
  , lastFocus :: Maybe WindowId
  }
  deriving stock (Eq, Show)

data World = World
  { windows :: Map WindowId Tracked
  , spaces :: Map SpaceId Space
  , displays :: [Display]
  , hiddenApps :: Set Pid
  , focused :: Maybe WindowId
  -- ^ The focused tiled window, as far as Kineo knows.
  }
  deriving stock (Eq, Show)

emptyWorld :: World
emptyWorld =
  World
    { windows = Map.empty
    , spaces = Map.empty
    , displays = []
    , hiddenApps = Set.empty
    , focused = Nothing
    }

emptySpace :: Space
emptySpace = Space {strip = Strip.empty, scroll = 0, lastFocus = Nothing}

data Event
  = WindowAppeared WindowInfo
  | WindowGone WindowId
  | WindowFocused WindowId
  | WindowMinimized WindowId Bool
  | -- | The user resized a window to this many pixels wide.
    WindowResized WindowId Double
  | AppHidden Pid Bool
  | AppTerminated Pid
  | -- | Displays or spaces changed: the new displays, and the space each
    -- known window is now on.
    Reconfigured [Display] (Map WindowId SpaceId)
  | Command Command
  | -- | Something outside the world changed (such as the config); lay out again.
    Relayout
  deriving stock (Eq, Show)

data Effect
  = -- | Move windows to these frames. Always the complete layout of every
    -- visible space; the animator skips windows that are already in place.
    Arrange [Placement]
  | FocusWindow WindowId
  | -- | Forget cached frames, so the next 'Arrange' moves every window.
    ForgetFrames
  | -- | Read the config file again.
    LoadConfig
  | Shutdown
  deriving stock (Eq, Show)

-- | Could this window be tiled at all? Dialogs, sheets, panels and windows
-- that cannot be moved and resized are never touched.
tileable :: WindowInfo -> Bool
tileable i = i.standard && i.resizable && i.movable && not i.fullscreen

step :: Config -> Event -> World -> (World, [Effect])
step cfg ev w0 = case ev of
  Command ReloadConfig -> (w0, [LoadConfig])
  Command Quit -> (w0, [Shutdown])
  Command Retile -> let w = settle cfg w0 in (w, [ForgetFrames, Arrange (layoutAll cfg w)])
  _ ->
    let (w1, effects) = update cfg ev w0
        w2 = settle cfg w1
     in (w2, effects ++ [Arrange (layoutAll cfg w2)])

update :: Config -> Event -> World -> (World, [Effect])
update cfg ev w = case ev of
  WindowAppeared info -> (appear cfg info w, [])
  WindowGone wid -> (forget wid w, [])
  WindowFocused wid -> (focusOn wid w, [])
  WindowMinimized wid True -> (hideWindow wid (setTracked wid (\t -> t {isMinimized = True}) w), [])
  WindowMinimized wid False -> (setTracked wid (\t -> t {isMinimized = False}) w, [])
  WindowResized wid px -> (resized cfg wid px w, [])
  AppHidden pid hidden ->
    ( w {hiddenApps = (if hidden then Set.insert else Set.delete) pid w.hiddenApps}
    , []
    )
  AppTerminated pid ->
    (foldr forget w (Map.keys (Map.filter (\t -> t.owner == pid) w.windows)), [])
  Reconfigured ds spaceOf -> (Map.foldrWithKey relocate w {displays = ds} spaceOf, [])
  Command c -> command cfg c w
  Relayout -> (w, [])

-- Windows ---------------------------------------------------------------

appear :: Config -> WindowInfo -> World -> World
appear cfg info w
  | Map.member info.wid w.windows || not (tileable info) = w
  | otherwise =
      let rule = ruleFor cfg info.bundleId info.title
          floating = maybe False (.float) rule
          sid = spaceForNew w info
          tracked =
            Tracked
              { owner = info.pid
              , onSpace = sid
              , isMinimized = info.minimized
              , isFloating = floating
              , originX = info.bounds.x
              }
          width = fromMaybe cfg.defaultWidth (rule >>= (.ruleWidth))
          w' = w {windows = Map.insert info.wid tracked w.windows}
       in if floating then w' else addToStrip width info.wid sid w'

-- | New windows usually appear on the active space, but the platform may
-- not know yet; fall back to the display the window is on.
spaceForNew :: World -> WindowInfo -> SpaceId
spaceForNew w info
  | info.space /= 0 = info.space
  | otherwise =
      let centre = (centerX info.bounds, centerY info.bounds)
       in case filter (\d -> contains d.frame centre) w.displays ++ w.displays of
            (d : _) -> d.currentSpace
            [] -> 0

-- | Put a window into a space's strip: after the focused window when it is
-- on that space, otherwise ordered by where the window was on screen.
addToStrip :: Double -> WindowId -> SpaceId -> World -> World
addToStrip width wid sid w = modifySpace sid insert w
  where
    col = Strip.column width wid
    anchor sp = case w.focused of
      Just f | onSpaceOf f == Just sid, Strip.member f sp.strip -> Just f
      _ -> sp.lastFocus >>= \f -> if Strip.member f sp.strip then Just f else Nothing
    onSpaceOf f = (.onSpace) <$> Map.lookup f w.windows
    insert sp = sp {strip = maybe (byOrigin sp.strip) (\a -> Strip.insertAfter (Just a) col sp.strip) (anchor sp)}
    byOrigin s =
      let x = maybe 0 (.originX) (Map.lookup wid w.windows)
          before c = maybe True (\t -> t.originX <= x) (Map.lookup (NE.head c.stack) w.windows)
       in Strip.insertAt (length (takeWhile before (Strip.columns s))) col s

-- | Stop managing a window entirely.
forget :: WindowId -> World -> World
forget wid w = case Map.lookup wid w.windows of
  Nothing -> w
  Just t ->
    let w' = hideWindow wid w
     in modifySpace t.onSpace (\sp -> sp {strip = Strip.remove wid sp.strip}) w' {windows = Map.delete wid w'.windows}

-- | A window is going out of view (closed, minimised, floated). If it held
-- focus, hand focus to its visible neighbour so keyboard navigation carries
-- on from a sensible place; macOS will report its own choice shortly after.
hideWindow :: WindowId -> World -> World
hideWindow wid w = case Map.lookup wid w.windows of
  Nothing -> w
  Just t ->
    let vis = visibleStrip w t.onSpace
        next = listToMaybe (mapMaybe (\d -> Strip.neighbor d wid vis) [DirLeft, DirRight, DirUp, DirDown])
        fixSpace sp = if sp.lastFocus == Just wid then sp {lastFocus = next} else sp
        w' = modifySpace t.onSpace fixSpace w
     in if w.focused == Just wid then w' {focused = next} else w'

focusOn :: WindowId -> World -> World
focusOn wid w = case Map.lookup wid w.windows of
  Just t
    | not t.isFloating ->
        -- A window that has focus is evidently neither minimised nor hidden,
        -- whatever notifications we might have missed.
        modifySpace t.onSpace (\sp -> sp {lastFocus = Just wid}) $
          w
            { focused = Just wid
            , windows = Map.insert wid t {isMinimized = False} w.windows
            , hiddenApps = Set.delete t.owner w.hiddenApps
            }
  _ -> w

resized :: Config -> WindowId -> Double -> World -> World
resized cfg wid px w = fromMaybe w $ do
  t <- Map.lookup wid w.windows
  d <- displayShowing w t.onSpace
  let frac = max 0.05 (min 1 (fractionFor cfg.layout (usableWidth cfg.layout d.visibleFrame) px))
  pure (modifyColumn wid (\c -> c {width = frac, savedWidth = Nothing}) w)

-- | A window has moved to another space (dragged in Mission Control, or
-- its space changed display).
relocate :: WindowId -> SpaceId -> World -> World
relocate wid sid w = case Map.lookup wid w.windows of
  Just t
    | sid /= 0 && sid /= t.onSpace ->
        let width = maybe 0.5 (.width) (Strip.columnOf wid =<< fmap (.strip) (Map.lookup t.onSpace w.spaces))
            w' = hideWindow wid w
            w'' =
              modifySpace t.onSpace (\sp -> sp {strip = Strip.remove wid sp.strip}) $
                w' {windows = Map.insert wid t {onSpace = sid} w'.windows}
         in if t.isFloating then w'' else addToStrip width wid sid w''
  _ -> w

-- Commands --------------------------------------------------------------

command :: Config -> Command -> World -> (World, [Effect])
command cfg c w = case c of
  Focus dir -> focusTo (\s cur -> Strip.neighbor dir cur s)
  FocusFirst -> focusTo (\s _ -> Strip.firstWindow s)
  FocusLast -> focusTo (\s _ -> Strip.lastWindow s)
  Move dir -> withCurrent $ \sid cur ->
    let move = if dir `elem` [DirLeft, DirRight] then Strip.moveColumn else Strip.moveInColumn
     in (modifySpace sid (\sp -> sp {strip = move (isVisible w) dir cur sp.strip}) w, [])
  CycleWidth -> withCurrent $ \_ cur -> (modifyColumn cur (cycleWidth cfg.widths True) w, [])
  CycleWidthBack -> withCurrent $ \_ cur -> (modifyColumn cur (cycleWidth cfg.widths False) w, [])
  ToggleFullWidth -> withCurrent $ \_ cur -> (modifyColumn cur fullWidth w, [])
  CenterColumn -> withCurrent $ \sid _ -> (scrollSpace cfg Center sid w, [])
  Consume -> withCurrent $ \sid cur -> (modifySpace sid (\sp -> sp {strip = Strip.consume (isVisible w) cur sp.strip}) w, [])
  Expel -> withCurrent $ \sid cur -> (modifySpace sid (\sp -> sp {strip = Strip.expel cur sp.strip}) w, [])
  ToggleFloat -> (toggleFloat cfg w, [])
  Retile -> (w, [])
  ReloadConfig -> (w, [])
  Quit -> (w, [])
  where
    withCurrent f = maybe (w, []) (uncurry f) (current w)
    focusTo pick = fromMaybe (w, []) $ do
      (sid, cur) <- current w
      target <- pick (visibleStrip w sid) cur
      pure (focusOn target w, [FocusWindow target | target /= cur])

-- | The window commands act on: the focused tiled window if it is visible,
-- otherwise the last focused window of the first display that has one.
current :: World -> Maybe (SpaceId, WindowId)
current w = case w.focused >>= \f -> (,f) . (.onSpace) <$> Map.lookup f w.windows of
  Just (sid, f) | Strip.member f (visibleStrip w sid) -> Just (sid, f)
  _ -> listToMaybe (mapMaybe fallback (shownSpaces w))
  where
    fallback (_, sid) =
      let vis = visibleStrip w sid
          lf = (Map.lookup sid w.spaces >>= (.lastFocus))
       in (sid,) <$> case lf of
            Just f | Strip.member f vis -> Just f
            _ -> Strip.firstWindow vis

cycleWidth :: [Double] -> Bool -> Column -> Column
cycleWidth presets forward c = c {width = next, savedWidth = Nothing}
  where
    eps = 0.01
    next
      | forward = case filter (> c.width + eps) presets of
          (p : _) -> p
          [] -> fromMaybe c.width (listToMaybe presets)
      | otherwise = case reverse (filter (< c.width - eps) presets) of
          (p : _) -> p
          [] -> fromMaybe c.width (listToMaybe (reverse presets))

fullWidth :: Column -> Column
fullWidth c = case c.savedWidth of
  Just s -> c {width = s, savedWidth = Nothing}
  Nothing -> c {width = 1, savedWidth = Just c.width}

toggleFloat :: Config -> World -> World
toggleFloat cfg w = fromMaybe w $ do
  f <- w.focused
  t <- Map.lookup f w.windows
  if t.isFloating
    then pure (addToStrip cfg.defaultWidth f t.onSpace (setTracked f (\x -> x {isFloating = False}) w))
    else
      let w' = hideWindow f w
       in pure $
            modifySpace t.onSpace (\sp -> sp {strip = Strip.remove f sp.strip}) $
              -- Keep it as the focused window so toggling again tiles it.
              (setTracked f (\x -> x {isFloating = True}) w') {focused = Just f}

-- Layout ----------------------------------------------------------------

isVisible :: World -> WindowId -> Bool
isVisible w wid = case Map.lookup wid w.windows of
  Just t -> not t.isMinimized && not t.isFloating && not (Set.member t.owner w.hiddenApps)
  Nothing -> False

-- | A space's strip without minimised windows and windows of hidden apps.
visibleStrip :: World -> SpaceId -> Strip
visibleStrip w sid = maybe Strip.empty (Strip.restrict (isVisible w) . (.strip)) (Map.lookup sid w.spaces)

-- | Displays paired with the space each shows. When displays share a space
-- (\"Displays have separate Spaces\" is off) only the first shows it, so no
-- window is placed twice.
shownSpaces :: World -> [(Display, SpaceId)]
shownSpaces w = go Set.empty w.displays
  where
    go _ [] = []
    go seen (d : ds)
      | not d.userSpace || Set.member d.currentSpace seen = go seen ds
      | otherwise = (d, d.currentSpace) : go (Set.insert d.currentSpace seen) ds

displayShowing :: World -> SpaceId -> Maybe Display
displayShowing w sid = listToMaybe [d | (d, s) <- shownSpaces w, s == sid]

-- | Keep each shown space scrolled so its focused column is in view.
settle :: Config -> World -> World
settle cfg w = foldr (\(_, sid) -> scrollSpace cfg cfg.focusMode sid) w (shownSpaces w)

scrollSpace :: Config -> FocusMode -> SpaceId -> World -> World
scrollSpace cfg mode sid w = fromMaybe w $ do
  d <- displayShowing w sid
  sp <- Map.lookup sid w.spaces
  let vis = visibleStrip w sid
      cols = Strip.columns vis
      focusCol = fst <$> (sp.lastFocus >>= \f -> Strip.locate f vis)
      scroll' = scrollFor cfg.layout mode (usableWidth cfg.layout d.visibleFrame) (map (.width) cols) focusCol sp.scroll
  pure w {spaces = Map.insert sid sp {scroll = scroll'} w.spaces}

-- | Where every window on every shown space belongs.
layoutAll :: Config -> World -> [Placement]
layoutAll cfg w = concat [layoutOn cfg w d sid | (d, sid) <- shownSpaces w]

layoutOn :: Config -> World -> Display -> SpaceId -> [Placement]
layoutOn cfg w d sid =
  let others = [o.frame | o <- w.displays, o.displayId /= d.displayId]
      scroll = maybe 0 (.scroll) (Map.lookup sid w.spaces)
   in place cfg.layout d.frame d.visibleFrame others scroll (visibleStrip w sid)

-- | Parked windows pulled fully back onto their display, for handing the
-- screen back to the user when Kineo exits.
released :: Config -> World -> [(WindowId, Rect)]
released cfg w =
  [ (p.window, unpark d.visibleFrame p.rect)
  | (d, sid) <- shownSpaces w
  , p <- layoutOn cfg w d sid
  , not p.onScreen
  ]

-- Helpers ---------------------------------------------------------------

modifySpace :: SpaceId -> (Space -> Space) -> World -> World
modifySpace sid f w = w {spaces = Map.alter (Just . f . fromMaybe emptySpace) sid w.spaces}

setTracked :: WindowId -> (Tracked -> Tracked) -> World -> World
setTracked wid f w = w {windows = Map.adjust f wid w.windows}

modifyColumn :: WindowId -> (Column -> Column) -> World -> World
modifyColumn wid f w = case Map.lookup wid w.windows of
  Just t | not t.isFloating -> modifySpace t.onSpace (\sp -> sp {strip = Strip.adjustColumn wid f sp.strip}) w
  _ -> w
