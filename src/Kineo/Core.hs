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
  , Workspace (..)
  , Display (..)
  , WindowInfo (..)
  , SpaceId
  , Pid
  , emptyWorld
  , emptySpace
  , emptyWorkspace
  , activeWorkspace

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

import Data.Foldable (toList)
import Data.Int (Int32)
import Data.List.NonEmpty qualified as NE
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import Data.Sequence (Seq, (<|), (|>))
import Data.Sequence qualified as Seq
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Word (Word32, Word64)
import Kineo.Command (Command (..))
import Kineo.Config (Config (..), Rule (..), ruleFor)
import Kineo.Geometry (Rect (..), centerX, centerY, contains)
import Kineo.Layout (FocusMode (..), Placement (..), fractionFor, place, scrollFor, stow, unpark, usableWidth)
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
-- strip: the strip of one of the workspaces of its space.
data Tracked = Tracked
  { owner :: Pid
  , onSpace :: SpaceId
  , isMinimized :: Bool
  , isFloating :: Bool
  , originX :: Double
  -- ^ Where the window was when discovered, to keep startup order natural.
  , minWidth :: Double
  -- ^ Pixels the app won't let the window be narrower than; 0 if unknown.
  , tabOf :: Maybe WindowId
  -- ^ A native tab that isn't selected: hidden behind this tab of the same
  -- window, which holds their place. Hidden tabs are in no strip.
  }
  deriving stock (Eq, Show)

-- | One strip and how it is scrolled. A space holds a vertical stack of
-- these; only the active one is on screen.
data Workspace = Workspace
  { strip :: Strip
  , scroll :: Double
  -- ^ Strip coordinate at the left edge of the usable area.
  , lastFocus :: Maybe WindowId
  }
  deriving stock (Eq, Show)

-- | A macOS space: workspaces stacked top to bottom. There is always at
-- least one, and every workspace but the active one holds a window; empty
-- ones are dropped as soon as they are left.
data Space = Space
  { workspaces :: Seq Workspace
  , active :: Int
  }
  deriving stock (Eq, Show)

data World = World
  { windows :: Map WindowId Tracked
  , spaces :: Map SpaceId Space
  , displays :: [Display]
  , hiddenApps :: Set Pid
  , focused :: Maybe WindowId
  -- ^ The focused tiled window, as far as Kineo knows.
  , blankFocus :: Maybe SpaceId
  -- ^ Set while the user is on an empty workspace: the space it belongs to.
  -- No window has focus then.
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
    , blankFocus = Nothing
    }

emptyWorkspace :: Workspace
emptyWorkspace = Workspace {strip = Strip.empty, scroll = 0, lastFocus = Nothing}

emptySpace :: Space
emptySpace = Space {workspaces = Seq.singleton emptyWorkspace, active = 0}

activeWorkspace :: Space -> Workspace
activeWorkspace sp = fromMaybe emptyWorkspace (Seq.lookup sp.active sp.workspaces)

data Event
  = WindowAppeared WindowInfo
  | WindowGone WindowId
  | WindowFocused WindowId
  | WindowMinimized WindowId Bool
  | -- | The user resized a window to this many pixels wide.
    WindowResized WindowId Double
  | -- | A window refused to be made narrower than this many pixels.
    WindowMinWidth WindowId Double
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
  | -- | Take keyboard focus away from every window: an empty workspace is
    -- on screen, and keys must not reach a window parked out of sight.
    FocusNothing
  | -- | Close a window as its close button would.
    Close WindowId
  | -- | Run a shell command in the background.
    Spawn String
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
  Command (Exec s) -> (w0, [Spawn s])
  Command Retile -> let w = settle cfg w0 in (w, [ForgetFrames, Arrange (layoutAll cfg w)])
  _ ->
    let (w1, effects) = update cfg ev w0
        (w2, moved) = tidy w1
        w3 = settle cfg w2
     in (w3, effects ++ moved ++ [Arrange (layoutAll cfg w3)])

update :: Config -> Event -> World -> (World, [Effect])
update cfg ev w = case ev of
  WindowAppeared info -> (appear cfg info w, [])
  WindowGone wid -> (forget wid w, [])
  WindowFocused wid -> (focusOn wid (revealTab wid w), [])
  WindowMinimized wid True -> (hideWindow wid (setTracked wid (\t -> t {isMinimized = True}) w), [])
  WindowMinimized wid False -> (setTracked wid (\t -> t {isMinimized = False}) w, [])
  -- Narrower than its supposed minimum by hand: that was a wrong guess.
  WindowResized wid px -> (resized cfg wid px (setTracked wid (\t -> if px < t.minWidth then t {minWidth = 0} else t) w), [])
  WindowMinWidth wid px -> (setTracked wid (\t -> t {minWidth = max t.minWidth px}) w, [])
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
  | Just (shown, t) <- tabbedWith cfg info w =
      showTab info.wid shown w {windows = Map.insert info.wid t {originX = info.bounds.x} w.windows}
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
              , minWidth = 0
              , tabOf = Nothing
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

-- | Put a window into the active workspace of a space.
addToStrip :: Double -> WindowId -> SpaceId -> World -> World
addToStrip width wid sid w = modifyActive sid (insertInto w width wid) w

-- | Put a window into a workspace: after the focused window when it is
-- there, otherwise after the workspace's last focused window, otherwise
-- ordered by where the window was on screen.
insertInto :: World -> Double -> WindowId -> Workspace -> Workspace
insertInto w width wid ws = ws {strip = maybe byOrigin (\a -> Strip.insertAfter (Just a) col ws.strip) anchor}
  where
    col = Strip.column width wid
    anchor = case w.focused of
      Just f | Strip.member f ws.strip -> Just f
      _ -> ws.lastFocus >>= \f -> if Strip.member f ws.strip then Just f else Nothing
    byOrigin =
      let x = maybe 0 (.originX) (Map.lookup wid w.windows)
          before c = maybe True (\t -> t.originX <= x) (Map.lookup (NE.head c.stack) w.windows)
       in Strip.insertAt (length (takeWhile before (Strip.columns ws.strip))) col ws.strip

-- | The window a new one is a native tab of, and how that is tracked:
-- macOS opens a tab exactly over the window it joins, where a new window
-- would be offset from it.
tabbedWith :: Config -> WindowInfo -> World -> Maybe (WindowId, Tracked)
tabbedWith cfg info w =
  listToMaybe
    [ (p.window, t)
    | p <- layoutAll cfg w
    , p.onScreen
    , near p.rect info.bounds
    , Just t <- [Map.lookup p.window w.windows]
    , t.owner == info.pid
    ]
  where
    -- Apps report whole pixels; layouts aren't.
    near a b = all (< 2) [abs (a.x - b.x), abs (a.y - b.y), abs (a.w - b.w), abs (a.h - b.h)]

-- | Show a window in the place of another of its tabs, which then hides
-- behind it along with the tabs that were hidden behind it.
showTab :: WindowId -> WindowId -> World -> World
showTab new old w = case (Map.lookup new w.windows, Map.lookup old w.windows) of
  (Just n, Just o) ->
    let retab t = if t.tabOf == Just old then t {tabOf = Just new} else t
        windows' =
          Map.insert new n {tabOf = Nothing, onSpace = o.onSpace, isFloating = o.isFloating}
            . Map.insert old o {tabOf = Just new}
            $ Map.map retab w.windows
        swap x = if x == Just old then Just new else x
        swapIn ws = ws {strip = Strip.replace old new ws.strip, lastFocus = swap ws.lastFocus}
     in modifySpace o.onSpace (\sp -> sp {workspaces = fmap swapIn sp.workspaces}) $
          w {windows = windows', focused = swap w.focused}
  _ -> w

-- | A hidden tab that takes focus has been selected: it takes its place.
revealTab :: WindowId -> World -> World
revealTab wid w = case Map.lookup wid w.windows >>= (.tabOf) of
  Just shown -> showTab wid shown w
  Nothing -> w

-- | Stop managing a window entirely. Closing the tab that is shown brings
-- another tab of the window forward in its place.
forget :: WindowId -> World -> World
forget wid w = case Map.lookup wid w.windows of
  Nothing -> w
  Just t
    | isNothing t.tabOf
    , (h : _) <- [h | (h, x) <- Map.toList w.windows, x.tabOf == Just wid] ->
        let w' = showTab h wid w in w' {windows = Map.delete wid w'.windows}
  Just t ->
    let w' = hideWindow wid w
     in removeFromSpace wid t.onSpace w' {windows = Map.delete wid w'.windows}

-- | A window is going out of view (closed, minimised, floated). If it held
-- focus, hand focus to its visible neighbour so keyboard navigation carries
-- on from a sensible place; macOS will report its own choice shortly after.
hideWindow :: WindowId -> World -> World
hideWindow wid w = case homeOf wid w of
  -- A floating window: nothing to hand focus to.
  Nothing -> if w.focused == Just wid then w {focused = Nothing} else w
  Just ws ->
    let vis = Strip.restrict (isVisible w) ws.strip
        next = listToMaybe (mapMaybe (\d -> Strip.neighbor d wid vis) [DirLeft, DirRight, DirUp, DirDown])
        fix x = if x.lastFocus == Just wid then x {lastFocus = next} else x
        w' = modifyHome wid fix w
     in if w.focused == Just wid then w' {focused = next} else w'

-- | Focus a window, bringing its workspace on screen.
focusOn :: WindowId -> World -> World
focusOn wid w = case Map.lookup wid w.windows of
  Just t
    | not t.isFloating ->
        -- A window that has focus is evidently neither minimised nor hidden,
        -- whatever notifications we might have missed.
        modifySpace t.onSpace activate $
          w
            { focused = Just wid
            , blankFocus = Nothing
            , windows = Map.insert wid t {isMinimized = False} w.windows
            , hiddenApps = Set.delete t.owner w.hiddenApps
            }
  _ -> w
  where
    activate sp = case workspaceIndex wid sp of
      Just i -> sp {active = i, workspaces = Seq.adjust' (\ws -> ws {lastFocus = Just wid}) i sp.workspaces}
      Nothing -> sp

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
    -- A hidden tab follows the tab it hides behind when that is shown.
    | sid /= 0 && sid /= t.onSpace && isNothing t.tabOf ->
        let width = maybe 0.5 (.width) (Strip.columnOf wid . (.strip) =<< homeOf wid w)
            w' = hideWindow wid w
            w'' = removeFromSpace wid t.onSpace w' {windows = Map.insert wid t {onSpace = sid} w'.windows}
         in if t.isFloating then w'' else addToStrip width wid sid w''
  _ -> w

-- Commands --------------------------------------------------------------

command :: Config -> Command -> World -> (World, [Effect])
command cfg c w = case c of
  Focus dir
    | vertical dir -> case current w of
        Just (sid, cur) -> case Strip.neighbor dir cur (visibleStrip w sid) of
          Just target -> focusFrom cur target
          Nothing -> switchWorkspace dir sid w
        Nothing -> maybe (w, []) (\sid -> switchWorkspace dir sid w) (blankSpace w)
    | otherwise -> withCurrent $ \sid cur ->
        maybe (w, []) (focusFrom cur) (Strip.neighbor dir cur (visibleStrip w sid))
  CloseWindow -> withCurrent $ \_ cur -> (w, [Close cur])
  Exec _ -> (w, [])
  FocusFirst -> focusTo Strip.firstWindow
  FocusLast -> focusTo Strip.lastWindow
  Move dir -> withCurrent $ \sid cur ->
    if vertical dir
      then case Strip.neighbor dir cur (visibleStrip w sid) of
        Just _ -> (modifyActive sid (\ws -> ws {strip = Strip.moveInColumn (isVisible w) dir cur ws.strip}) w, [])
        Nothing -> (moveToWorkspace cfg dir sid cur w, [])
      else (modifyActive sid (\ws -> ws {strip = Strip.moveColumn (isVisible w) dir cur ws.strip}) w, [])
  CycleWidth -> withCurrent $ \_ cur -> (modifyColumn cur (cycleWidth cfg.widths True) w, [])
  CycleWidthBack -> withCurrent $ \_ cur -> (modifyColumn cur (cycleWidth cfg.widths False) w, [])
  ToggleFullWidth -> withCurrent $ \_ cur -> (modifyColumn cur fullWidth w, [])
  CenterColumn -> withCurrent $ \sid _ -> (scrollSpace cfg Center sid w, [])
  Consume -> withCurrent $ \sid cur -> (modifyActive sid (\ws -> ws {strip = Strip.consume (isVisible w) cur ws.strip}) w, [])
  Expel -> withCurrent $ \sid cur -> (modifyActive sid (\ws -> ws {strip = Strip.expel cur ws.strip}) w, [])
  ToggleFloat -> (toggleFloat cfg w, [])
  Retile -> (w, [])
  ReloadConfig -> (w, [])
  Quit -> (w, [])
  where
    vertical dir = dir == DirUp || dir == DirDown
    withCurrent f = maybe (w, []) (uncurry f) (current w)
    focusFrom cur target = (focusOn target w, [FocusWindow target | target /= cur])
    focusTo pick = fromMaybe (w, []) $ do
      (sid, cur) <- current w
      target <- pick (visibleStrip w sid)
      pure (focusFrom cur target)

-- | The window commands act on: the focused tiled window if it is visible
-- on its space's active workspace, otherwise the last focused window of the
-- first display that has one. None while the user is on an empty workspace.
current :: World -> Maybe (SpaceId, WindowId)
current w = case w.focused >>= \f -> (,f) . (.onSpace) <$> Map.lookup f w.windows of
  Just (sid, f) | Strip.member f (visibleStrip w sid) -> Just (sid, f)
  _ | Just _ <- blankSpace w -> Nothing
  _ -> listToMaybe (mapMaybe (\(_, sid) -> (sid,) <$> (workspaceFocus w . activeWorkspace =<< Map.lookup sid w.spaces)) (shownSpaces w))

-- | The space whose empty workspace the user is on, if it is still shown
-- and still has nothing to focus.
blankSpace :: World -> Maybe SpaceId
blankSpace w = do
  sid <- w.blankFocus
  sp <- Map.lookup sid w.spaces
  if isJust (displayShowing w sid) && isNothing (workspaceFocus w (activeWorkspace sp))
    then Just sid
    else Nothing

-- | The window to focus on arriving at a workspace: its last focused
-- window if still visible, otherwise its first visible window.
workspaceFocus :: World -> Workspace -> Maybe WindowId
workspaceFocus w ws = case ws.lastFocus of
  Just f | Strip.member f vis -> Just f
  _ -> Strip.firstWindow vis
  where
    vis = Strip.restrict (isVisible w) ws.strip

-- | Go to the workspace above or below the active one. Past the first or
-- last there is always an empty one to go to, unless the active workspace
-- is already empty.
switchWorkspace :: Dir -> SpaceId -> World -> (World, [Effect])
switchWorkspace dir sid w = fromMaybe (w, []) $ do
  sp <- Map.lookup sid w.spaces
  let n = Seq.length sp.workspaces
      i = if dir == DirUp then sp.active - 1 else sp.active + 1
      blank = null (Strip.windows (activeWorkspace sp).strip)
      arrive s j =
        let w' = w {spaces = Map.insert sid s {active = j} w.spaces}
         in case workspaceFocus w' =<< Seq.lookup j s.workspaces of
              Just t -> (focusOn t w', [FocusWindow t])
              Nothing -> (w' {focused = Nothing, blankFocus = Just sid}, [FocusNothing])
      go
        | i >= 0 && i < n = Just (arrive sp i)
        | blank = Nothing
        | dir == DirUp = Just (arrive sp {workspaces = emptyWorkspace <| sp.workspaces} 0)
        | otherwise = Just (arrive sp {workspaces = sp.workspaces |> emptyWorkspace} n)
  go

-- | Take a window to the workspace above or below, starting a new
-- workspace when there is none, and follow it there. A window alone on its
-- workspace stays put rather than moving to a new, equally empty one.
moveToWorkspace :: Config -> Dir -> SpaceId -> WindowId -> World -> World
moveToWorkspace cfg dir sid wid w = fromMaybe w $ do
  sp <- Map.lookup sid w.spaces
  let n = Seq.length sp.workspaces
      src = activeWorkspace sp
      width = maybe cfg.defaultWidth (.width) (Strip.columnOf wid src.strip)
      atEdge = if dir == DirUp then sp.active == 0 else sp.active == n - 1
      alone = Strip.windows src.strip == [wid]
      -- Room made for the window, and the index it goes to afterwards.
      (grow, target)
        | dir == DirUp && atEdge = (\s -> s {workspaces = emptyWorkspace <| s.workspaces, active = s.active + 1}, 0)
        | dir == DirUp = (id, sp.active - 1)
        | atEdge = (\s -> s {workspaces = s.workspaces |> emptyWorkspace}, n)
        | otherwise = (id, sp.active + 1)
  if atEdge && alone
    then Nothing
    else do
      let w1 = modifySpace sid grow (removeFromSpace wid sid (hideWindow wid w))
          w2 = modifySpace sid (\s -> s {workspaces = Seq.adjust' (insertInto w1 width wid) target s.workspaces}) w1
      pure (focusOn wid w2)

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
            removeFromSpace f t.onSpace $
              -- Keep it as the focused window so toggling again tiles it.
              (setTracked f (\x -> x {isFloating = True}) w') {focused = Just f}

-- Workspaces ------------------------------------------------------------

-- | Keep every space's workspaces in order after an event: when the active
-- workspace has nothing left to show, go to the nearest one that has
-- (above first), taking focus along if focus was on this space; then drop
-- the empty workspaces that are not active.
tidy :: World -> (World, [Effect])
tidy w0 = (w1 {spaces = Map.map prune w1.spaces}, effects)
  where
    (w1, effects) = foldl' leave (w0, []) (Map.keys w0.spaces)

    leave (w, es) sid = fromMaybe (w, es) $ do
      sp <- Map.lookup sid w.spaces
      -- An empty workspace the user went to on purpose stays.
      if isNothing (workspaceFocus w (activeWorkspace sp)) && w.blankFocus /= Just sid
        then do
          let a = sp.active
              order = concat [[a - k, a + k] | k <- [1 .. Seq.length sp.workspaces]]
          (i, target) <- listToMaybe [(i, t) | i <- order, Just ws <- [Seq.lookup i sp.workspaces], Just t <- [workspaceFocus w ws]]
          let w' = w {spaces = Map.insert sid sp {active = i} w.spaces}
              -- Not when a floating window here, or any window elsewhere, has focus.
              focusHere = case w.focused >>= \f -> Map.lookup f w.windows of
                Just t -> t.onSpace == sid && not t.isFloating
                Nothing -> True
          pure (if focusHere then (focusOn target w', es ++ [FocusWindow target]) else (w', es))
        else Nothing

    prune sp =
      let kept = [(i, ws) | (i, ws) <- zip [0 ..] (toList sp.workspaces), i == sp.active || not (null (Strip.windows ws.strip))]
       in Space
            { workspaces = Seq.fromList (map snd kept)
            , active = length (takeWhile ((< sp.active) . fst) kept)
            }

-- Layout ----------------------------------------------------------------

isVisible :: World -> WindowId -> Bool
isVisible w wid = case Map.lookup wid w.windows of
  Just t -> not t.isMinimized && not t.isFloating && not (Set.member t.owner w.hiddenApps)
  Nothing -> False

-- | The active workspace's strip on a space, without minimised windows and
-- windows of hidden apps.
visibleStrip :: World -> SpaceId -> Strip
visibleStrip w sid = maybe Strip.empty (Strip.restrict (isVisible w) . (.strip) . activeWorkspace) (Map.lookup sid w.spaces)

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
  let ws = activeWorkspace sp
      vis = widened cfg w d (visibleStrip w sid)
      cols = Strip.columns vis
      focusCol = fst <$> (ws.lastFocus >>= \f -> Strip.locate f vis)
      scroll' = scrollFor cfg.layout mode (usableWidth cfg.layout d.visibleFrame) (map (.width) cols) focusCol ws.scroll
  pure (modifyActive sid (\x -> x {scroll = scroll'}) w)

-- | Where every window on every shown space belongs.
layoutAll :: Config -> World -> [Placement]
layoutAll cfg w = concat [layoutOn cfg w d sid | (d, sid) <- shownSpaces w]

-- | The active workspace laid out on the display, and every other
-- workspace's windows stowed below it.
layoutOn :: Config -> World -> Display -> SpaceId -> [Placement]
layoutOn cfg w d sid =
  let others = [o.frame | o <- w.displays, o.displayId /= d.displayId]
      sp = fromMaybe emptySpace (Map.lookup sid w.spaces)
      vis ws = widened cfg w d (Strip.restrict (isVisible w) ws.strip)
      lay i ws
        | i == sp.active = place cfg.layout d.frame d.visibleFrame others ws.scroll (vis ws)
        | otherwise = stow cfg.layout d.frame d.visibleFrame others ws.scroll (vis ws)
   in concat (zipWith lay [0 ..] (toList sp.workspaces))

-- | A strip with every column at least as wide as its widest window's
-- minimum, so a window that won't shrink never overlaps its neighbours.
-- Only for laying out: the column keeps its chosen width, to return to if
-- the window moves elsewhere.
widened :: Config -> World -> Display -> Strip -> Strip
widened cfg w d = Strip.fromColumns . map widen . Strip.columns
  where
    usable = usableWidth cfg.layout d.visibleFrame
    minOf wid = maybe 0 (.minWidth) (Map.lookup wid w.windows)
    widen c = case maximum (fmap minOf c.stack) of
      m | m > 0 -> c {width = max c.width (fractionFor cfg.layout usable m)}
      _ -> c

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

modifyActive :: SpaceId -> (Workspace -> Workspace) -> World -> World
modifyActive sid f = modifySpace sid (\sp -> sp {workspaces = Seq.adjust' f sp.active sp.workspaces})

-- | Index of the workspace holding a window.
workspaceIndex :: WindowId -> Space -> Maybe Int
workspaceIndex wid sp = Seq.findIndexL (\ws -> Strip.member wid ws.strip) sp.workspaces

-- | The workspace holding a window.
homeOf :: WindowId -> World -> Maybe Workspace
homeOf wid w = do
  t <- Map.lookup wid w.windows
  sp <- Map.lookup t.onSpace w.spaces
  i <- workspaceIndex wid sp
  Seq.lookup i sp.workspaces

-- | Modify the workspace holding a window.
modifyHome :: WindowId -> (Workspace -> Workspace) -> World -> World
modifyHome wid f w = case Map.lookup wid w.windows of
  Just t -> modifySpace t.onSpace (\sp -> maybe sp (\i -> sp {workspaces = Seq.adjust' f i sp.workspaces}) (workspaceIndex wid sp)) w
  Nothing -> w

-- | Take a window out of every workspace of a space.
removeFromSpace :: WindowId -> SpaceId -> World -> World
removeFromSpace wid sid = modifySpace sid (\sp -> sp {workspaces = fmap (\ws -> ws {strip = Strip.remove wid ws.strip}) sp.workspaces})

setTracked :: WindowId -> (Tracked -> Tracked) -> World -> World
setTracked wid f w = w {windows = Map.adjust f wid w.windows}

modifyColumn :: WindowId -> (Column -> Column) -> World -> World
modifyColumn wid f w = case Map.lookup wid w.windows of
  Just t | not t.isFloating -> modifyHome wid (\ws -> ws {strip = Strip.adjustColumn wid f ws.strip}) w
  _ -> w
