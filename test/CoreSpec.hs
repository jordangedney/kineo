module CoreSpec (tests) where

import Data.Foldable (toList)
import Data.List (nub)
import Data.Map.Strict qualified as Map
import Gen
import Kineo.Command (Command (..))
import Kineo.Config (Config (..), defaultConfig)
import Kineo.Core
import Kineo.Geometry (Rect (..))
import Kineo.Layout (Params (..), Placement (..))
import Kineo.Strip (Dir (..), WindowId)
import Kineo.Strip qualified as Strip
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

cfg :: Config
cfg = defaultConfig

-- | Run events from a world with one laptop display, keeping the effects of
-- the last event.
run :: [Event] -> (World, [Effect])
run = foldl' (\(w, _) e -> step cfg e w) (fst (step cfg (Reconfigured [laptop] Map.empty) emptyWorld), [])

world :: [Event] -> World
world = fst . run

order :: World -> [[WindowId]]
order w = maybe [] (\sp -> map (\c -> foldr (:) [] c.stack) (Strip.columns (activeWorkspace sp).strip)) (Map.lookup 1 w.spaces)

-- | The windows of each workspace on space 1, and which one is active.
stacks :: World -> ([[WindowId]], Int)
stacks w = maybe ([], 0) (\sp -> (map (Strip.windows . (.strip)) (toList sp.workspaces), sp.active)) (Map.lookup 1 w.spaces)

arranged :: [Effect] -> [WindowId]
arranged es = concat [map (.window) ps | Arrange ps <- es]

contains :: [Effect] -> Effect -> Assertion
contains es e = e `elem` es @? "expected " ++ show e ++ " in " ++ show es

open :: [WindowId] -> [Event]
open = map (WindowAppeared . window)

-- | After some events, a native tab @new@ opening exactly over @over@.
tab :: WindowId -> WindowId -> [Event] -> [Event]
tab new over evs = tabFrom (window over).pid new over evs

-- | The same, as a window of the app with this pid.
tabFrom :: Pid -> WindowId -> WindowId -> [Event] -> [Event]
tabFrom p new over evs = case [pl.rect | pl <- layoutAll cfg (world evs), pl.window == over] of
  [r] -> evs ++ [WindowAppeared (window new) {pid = p, bounds = r}]
  _ -> error ("window " ++ show over ++ " isn't laid out")

tests :: TestTree
tests =
  testGroup
    "Core"
    [ testCase "windows found at startup keep their left-to-right order" $
        order (world [WindowAppeared (window 1) {bounds = (window 1).bounds {x = 900}}, WindowAppeared (window 2) {bounds = (window 2).bounds {x = 10}}])
          @?= [[2], [1]]
    , testCase "a new window opens to the right of the focused one" $
        order (world (open [1, 2, 3] ++ [WindowFocused 1, WindowAppeared (window 4)])) @?= [[1], [4], [2], [3]]
    , testCase "dialogs and other non-standard windows are ignored" $
        Map.member 5 (world [WindowAppeared (dialog 5)]).windows @?= False
    , testCase "rules can float a window" $ do
        let w = world [WindowAppeared (window 1) {bundleId = "com.apple.systempreferences"}]
        order w @?= []
        fmap (.isFloating) (Map.lookup 1 w.windows) @?= Just True
    , testCase "focus-right focuses the neighbour" $ do
        let (w, es) = run (open [1, 2, 3] ++ [WindowFocused 1, Command (Focus DirRight)])
        w.focused @?= Just 2
        FocusWindow 2 `elem` es @? "expected FocusWindow 2 in " ++ show es
    , testCase "closing the focused window hands focus to its left neighbour" $
        (world (open [1, 2, 3] ++ [WindowFocused 2, WindowGone 2])).focused @?= Just 1
    , testCase "minimised windows are skipped and come back in place" $ do
        let w = world (open [1, 2, 3] ++ [WindowFocused 1, WindowMinimized 2 True])
        (step cfg (Command (Focus DirRight)) w & fst).focused @?= Just 3
        arranged (snd (step cfg Relayout w)) @?= [1, 3]
        order (world (open [1, 2, 3] ++ [WindowMinimized 2 True, WindowMinimized 2 False])) @?= [[1], [2], [3]]
    , testCase "hidden apps' windows leave the layout" $ do
        let (_, es) = run (open [1, 2] ++ [WindowAppeared (window 3) {pid = 7}, AppHidden 7 True])
        arranged es @?= [1, 2]
    , testCase "quitting an app forgets its windows" $
        order (world (open [1, 2] ++ [WindowAppeared (window 3) {pid = 7}, AppTerminated 7])) @?= [[1], [2]]
    , testCase "move-left swaps columns" $
        order (world (open [1, 2, 3] ++ [WindowFocused 3, Command (Move DirLeft)])) @?= [[1], [3], [2]]
    , testCase "cycle-width steps through the presets and wraps" $ do
        let widthAfter n = do
              let w = world (open [1] ++ [WindowFocused 1] ++ replicate n (Command CycleWidth))
              sp <- Map.lookup 1 w.spaces
              (.width) <$> Strip.columnOf 1 (activeWorkspace sp).strip
        map widthAfter [0, 1, 2, 3] @?= map Just [0.5, 0.6667, 1, 0.3333]
    , testCase "toggle-full-width remembers the old width" $ do
        let w = world (open [1] ++ [WindowFocused 1, Command ToggleFullWidth, Command ToggleFullWidth])
        ((.width) <$> (Strip.columnOf 1 . (.strip) . activeWorkspace =<< Map.lookup 1 w.spaces)) @?= Just 0.5
    , testCase "resizing a window by hand changes its column width" $ do
        let w = world (open [1] ++ [WindowResized 1 400])
        fmap (< 0.5) ((.width) <$> (Strip.columnOf 1 . (.strip) . activeWorkspace =<< Map.lookup 1 w.spaces)) @?= Just True
    , testCase "a window that won't shrink widens its column instead of overlapping" $ do
        let (_, es) = run (open [1, 2] ++ [WindowFocused 1, WindowMinWidth 1 1000])
            near a b = abs (a - b) < 0.01
        case ([p.rect | Arrange ps <- es, p <- ps, p.window == 1], [p.rect | Arrange ps <- es, p <- ps, p.window == 2]) of
          ([r1], [r2]) -> do
            near r1.w 1000 @? "column 1 is " ++ show r1.w ++ " wide"
            near r2.x (r1.x + r1.w + cfg.layout.gap) @? "window 2 starts at " ++ show r2.x
          other -> assertFailure ("placements: " ++ show other)
        -- The column keeps its own width, for when the window leaves it.
        (Strip.columns . (.strip) . activeWorkspace <$> Map.lookup 1 (world (open [1, 2] ++ [WindowMinWidth 1 1000])).spaces)
          @?= Just [Strip.Column {stack = pure 1, width = 0.5, savedWidth = Nothing}, Strip.Column {stack = pure 2, width = 0.5, savedWidth = Nothing}]
    , testCase "a native tab takes its window's place instead of a new column" $ do
        let w = world (tab 3 1 (open [1, 2] ++ [WindowFocused 1]))
        order w @?= [[3], [2]]
        ((.tabOf) <$> Map.lookup 1 w.windows) @?= Just (Just 3)
    , testCase "a tab over a parked window is a tab too" $ do
        let evs = open [1, 2, 3] ++ [WindowFocused 3]
            parked = [p.window | p <- layoutAll cfg (world evs), not p.onScreen]
        parked @?= [1]
        order (world (tab 4 1 evs)) @?= [[4], [2], [3]]
    , testCase "selecting a hidden tab brings it back in place" $
        order (world (tab 3 1 (open [1, 2] ++ [WindowFocused 1]) ++ [WindowFocused 1])) @?= [[1], [2]]
    , testCase "closing the shown tab shows another in its place" $
        order (world (tab 3 1 (open [1, 2] ++ [WindowFocused 1]) ++ [WindowGone 3])) @?= [[1], [2]]
    , testCase "closing a hidden tab changes nothing" $ do
        let w = world (tab 3 1 (open [1, 2] ++ [WindowFocused 1]) ++ [WindowGone 1])
        order w @?= [[3], [2]]
        Map.member 1 w.windows @?= False
    , testCase "another app's window over a window is not a tab" $
        order (world (tabFrom 9 3 1 (open [1, 2] ++ [WindowFocused 1]))) @?= [[1], [3], [2]]
    , testCase "a window dragged to another space moves strips" $ do
        let w = world (open [1, 2] ++ [Reconfigured [laptop, external] (Map.fromList [(2, 2)])])
        order w @?= [[1]]
        (Strip.windows . (.strip) . activeWorkspace <$> Map.lookup 2 w.spaces) @?= Just [2]
    , testCase "toggle-float takes a window out and puts it back" $ do
        let w = world (open [1, 2] ++ [WindowFocused 2, Command ToggleFloat])
        order w @?= [[1]]
        order (fst (step cfg (Command ToggleFloat) w)) @?= [[1], [2]]
    , testCase "move-up at the top of a column starts a workspace above" $ do
        let w = world (open [1, 2, 3] ++ [WindowFocused 2, Command (Move DirUp)])
        stacks w @?= ([[2], [1, 3]], 0)
        w.focused @?= Just 2
    , testCase "focus-down and focus-up cross between workspaces" $ do
        let (w, es) = run (open [1, 2, 3] ++ [WindowFocused 2, Command (Move DirUp), Command (Focus DirDown)])
        (w.focused, snd (stacks w)) @?= (Just 1, 1)
        FocusWindow 1 `elem` es @? "expected FocusWindow 1 in " ++ show es
        let (w', es') = step cfg (Command (Focus DirUp)) w
        (w'.focused, snd (stacks w')) @?= (Just 2, 0)
        FocusWindow 2 `elem` es' @? "expected FocusWindow 2 in " ++ show es'
    , testCase "focus-up moves within a column before leaving the workspace" $ do
        let w = world (open [1, 2, 3] ++ [WindowFocused 3, Command (Move DirUp), Command (Focus DirDown), Command Consume])
        stacks w @?= ([[3], [1, 2]], 1)
        let w' = fst (step cfg (Command (Focus DirUp)) w)
        w'.focused @?= Just 1
        (fst (step cfg (Command (Focus DirUp)) w')).focused @?= Just 3
    , testCase "focus-down past the last workspace goes to an empty one" $ do
        let (w, es) = run (open [1, 2] ++ [WindowFocused 2, Command (Focus DirDown)])
        (w.focused, stacks w) @?= (Nothing, ([[1, 2], []], 1))
        es `contains` FocusNothing
        arranged es @?= [1, 2]
        -- Still there after unrelated events, and no further to go.
        let (w', es') = foldl' (\(x, _) e -> step cfg e x) (w, []) [Relayout, Command (Focus DirDown), Command (Focus DirLeft)]
        stacks w' @?= ([[1, 2], []], 1)
        [x | x@(FocusWindow _) <- es'] @?= []
    , testCase "focus-up past the first workspace goes to an empty one" $
        stacks (world (open [1] ++ [WindowFocused 1, Command (Focus DirUp)])) @?= ([[], [1]], 0)
    , testCase "leaving an empty workspace drops it" $ do
        let (w, es) = run (open [1, 2] ++ [WindowFocused 2, Command (Focus DirDown), Command (Focus DirUp)])
        (w.focused, stacks w) @?= (Just 2, ([[1, 2]], 0))
        es `contains` FocusWindow 2
    , testCase "a window opened on an empty workspace stays there" $ do
        let w = world (open [1, 2] ++ [WindowFocused 2, Command (Focus DirDown), WindowAppeared (window 3), WindowFocused 3])
        (w.focused, stacks w) @?= (Just 3, ([[1, 2], [3]], 1))
    , testCase "close closes the focused window" $
        snd (run (open [1, 2] ++ [WindowFocused 1, Command CloseWindow])) `contains` Close 1
    , testCase "exec runs its command" $
        snd (run [Command (Exec "open -a Foo")]) @?= [Spawn "open -a Foo"]
    , testCase "other workspaces are parked below the display" $ do
        let (_, es) = run (open [1, 2, 3] ++ [WindowFocused 2, Command (Move DirUp)])
            ps = concat [p | Arrange p <- es]
            parked = [p.window | p <- ps, not p.onScreen, p.rect.y == 900 - cfg.layout.sliver]
        parked @?= [1, 3]
    , testCase "closing the last window of a workspace returns to the one above" $ do
        let (w, es) = run (open [1, 2, 3] ++ [WindowFocused 3, Command (Move DirDown), WindowGone 3])
        (w.focused, stacks w) @?= (Just 2, ([[1, 2]], 0))
        FocusWindow 2 `elem` es @? "expected FocusWindow 2 in " ++ show es
    , testCase "focusing a window on another workspace brings it on screen" $
        snd (stacks (world (open [1, 2, 3] ++ [WindowFocused 3, Command (Move DirDown), WindowFocused 1]))) @?= 0
    , testCase "a window alone on its workspace does not start another" $
        stacks (world (open [1] ++ [WindowFocused 1, Command (Move DirDown)])) @?= ([[1]], 0)
    , testCase "new windows open on the active workspace" $
        stacks (world (open [1, 2, 3] ++ [WindowFocused 3, Command (Move DirDown), WindowAppeared (window 4)]))
          @?= ([[1, 2], [3, 4]], 1)
    , testCase "every window on a shown space is placed exactly once" $ do
        let (_, es) = run (open [1 .. 6])
        let ws = arranged es
        ws @?= nub ws
        length ws @?= 6
    , testProperty "invariants hold after any sequence of events" $
        forAll (listOf genStep) $ \steps ->
          let go w [] = invariant w
              go w (s : ss) = let (w', _) = step cfg (event w s) w in invariant w' >> go w' ss
           in case go emptyWorld steps of
                Right () -> property True
                Left err -> counterexample err False
    , testProperty "arrange only ever places visible windows, once each" $
        forAll (listOf genEvent) $ \evs ->
          let (w, es) = foldl' (\(x, _) e -> step cfg e x) (emptyWorld, []) evs
              ws = arranged es
           in ws === nub ws .&&. all (isVisible w) ws
    ]
  where
    (&) = flip ($)
    -- Random events, and now and then a native tab opening over one of the
    -- windows laid out at the time.
    genStep = frequency [(8, Left <$> genEvent), (1, Right <$> ((,) <$> chooseInt (0, 20) <*> chooseEnum (1, 12)))]
    event w = \case
      Left e -> e
      Right (i, new) -> case [p | p <- layoutAll cfg w, p.onScreen] of
        [] -> Relayout
        ps ->
          let p = ps !! (i `mod` length ps)
              owner = maybe 1 (.owner) (Map.lookup p.window w.windows)
           in WindowAppeared (window new) {pid = owner, bounds = p.rect}
