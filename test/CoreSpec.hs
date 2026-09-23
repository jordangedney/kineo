module CoreSpec (tests) where

import Data.List (nub)
import Data.Map.Strict qualified as Map
import Gen
import Kineo.Command (Command (..))
import Kineo.Config (Config (..), defaultConfig)
import Kineo.Core
import Kineo.Geometry (Rect (..))
import Kineo.Layout (Placement (..))
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
order w = maybe [] (\sp -> map (\c -> foldr (:) [] c.stack) (Strip.columns sp.strip)) (Map.lookup 1 w.spaces)

arranged :: [Effect] -> [WindowId]
arranged es = concat [map (.window) ps | Arrange ps <- es]

open :: [WindowId] -> [Event]
open = map (WindowAppeared . window)

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
              (.width) <$> Strip.columnOf 1 sp.strip
        map widthAfter [0, 1, 2, 3] @?= map Just [0.5, 0.6667, 1, 0.3333]
    , testCase "toggle-full-width remembers the old width" $ do
        let w = world (open [1] ++ [WindowFocused 1, Command ToggleFullWidth, Command ToggleFullWidth])
        ((.width) <$> (Strip.columnOf 1 . (.strip) =<< Map.lookup 1 w.spaces)) @?= Just 0.5
    , testCase "resizing a window by hand changes its column width" $ do
        let w = world (open [1] ++ [WindowResized 1 400])
        fmap (< 0.5) ((.width) <$> (Strip.columnOf 1 . (.strip) =<< Map.lookup 1 w.spaces)) @?= Just True
    , testCase "a window dragged to another space moves strips" $ do
        let w = world (open [1, 2] ++ [Reconfigured [laptop, external] (Map.fromList [(2, 2)])])
        order w @?= [[1]]
        (Strip.windows . (.strip) <$> Map.lookup 2 w.spaces) @?= Just [2]
    , testCase "toggle-float takes a window out and puts it back" $ do
        let w = world (open [1, 2] ++ [WindowFocused 2, Command ToggleFloat])
        order w @?= [[1]]
        order (fst (step cfg (Command ToggleFloat) w)) @?= [[1], [2]]
    , testCase "every window on a shown space is placed exactly once" $ do
        let (_, es) = run (open [1 .. 6])
        let ws = arranged es
        ws @?= nub ws
        length ws @?= 6
    , testProperty "invariants hold after any sequence of events" $
        forAll (listOf genEvent) $ \evs ->
          let go w [] = invariant w
              go w (e : es) = let (w', _) = step cfg e w in invariant w' >> go w' es
           in case go emptyWorld evs of
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
