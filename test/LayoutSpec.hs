module LayoutSpec (tests) where

import Kineo.Geometry (Rect (..), intersects, right)
import Kineo.Layout
import Kineo.Strip (Column (..), fromColumns)
import Data.List.NonEmpty qualified as NE
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

params :: Params
params = Params {gap = 10, margin = 20, sliver = 8}

full, visible :: Rect
full = Rect 0 0 1440 900
visible = Rect 0 25 1440 875

usable :: Double
usable = usableWidth params visible

cols :: [(Double, [Word])] -> [Column]
cols = map (\(wd, ws) -> Column (NE.fromList (map fromIntegral ws)) wd Nothing)

genFracs :: Gen [Double]
genFracs = listOf1 (elements [0.3333, 0.5, 0.6667, 1])

tests :: TestTree
tests =
  testGroup
    "Layout"
    [ testCase "two half columns exactly fill the usable width" $
        case spans params usable [0.5, 0.5] of
          [(x0, w0), (x1, w1)] -> do
            x0 @?= 0
            x1 + w1 @?= usable
            x1 - (x0 + w0) @?= params.gap
          other -> assertFailure (show other)
    , testProperty "fractionFor inverts columnPx" $
        forAll (choose (0.05, 1)) $ \f ->
          abs (fractionFor params usable (columnPx params usable f) - f) < 1e-9
    , testProperty "reveal brings the focused column fully into view" $
        forAll genFracs $ \fs ->
          forAll (chooseInt (0, length fs - 1)) $ \i ->
            forAll (choose (0, 5000)) $ \prev ->
              let s = scrollFor params Reveal usable fs (Just i) prev
                  (cx, cw) = spans params usable fs !! i
               in cw <= usable ==> cx >= s - 1e-6 && cx + cw <= s + usable + 1e-6
    , testProperty "scroll stays within the strip" $
        forAll genFracs $ \fs ->
          forAll (elements [Reveal, Center]) $ \mode ->
            forAll (chooseInt (0, length fs - 1)) $ \i ->
              let s = scrollFor params mode usable fs (Just i) 0
                  (lx, lw) = last (spans params usable fs)
               in s >= 0 && s <= max 0 (lx + lw - usable) + 1e-6
    , testCase "reveal does not scroll when the focused column is already visible" $
        scrollFor params Reveal usable [0.5, 0.5, 0.5] (Just 1) 0 @?= 0
    , testCase "stacked windows share the column height" $ do
        let ps = place params full visible [] 0 (fromColumns (cols [(0.5, [1, 2])]))
        map (.rect.h) ps @?= replicate 2 ((875 - 40 - 10) / 2)
        map (.rect.y) ps @?= [45, 45 + (875 - 40 - 10) / 2 + 10]
    , testCase "a small margin still leaves a gap beside the parked slivers" $ do
        let p = params {margin = 4}
        case place p full visible [] 0 (fromColumns (cols [(1, [1])])) of
          [a] -> do
            a.rect.x @?= full.x + p.sliver + p.gap
            right full - right a.rect @?= p.sliver + p.gap
          ps -> assertFailure (show ps)
    , testCase "windows past the right edge are parked with a sliver showing" $ do
        let ps = place params full visible [] 0 (fromColumns (cols [(1, [1]), (1, [2])]))
        case ps of
          [a, b] -> do
            a.onScreen @?= True
            b.onScreen @?= False
            b.rect.x @?= right full - params.sliver
          _ -> assertFailure (show ps)
    , testCase "windows scrolled off the left are parked on the left" $ do
        let ps = place params full visible [] 5000 (fromColumns (cols [(1, [1]), (1, [2])]))
        case ps of
          (a : _) -> right a.rect @?= full.x + params.sliver
          _ -> assertFailure (show ps)
    , testCase "a parked window never spills onto a neighbouring display" $ do
        let other = Rect 1440 0 1920 1080
            ps = place params full visible [other] 0 (fromColumns (cols [(1, [1]), (0.5, [2]), (0.5, [3])]))
        mapM_ (\p -> assertBool (show p) (not (intersects p.rect other))) ps
    , testCase "a partly visible column stays put when nothing is beside it" $ do
        let ps = place params full visible [] 0 (fromColumns (cols [(0.6667, [1]), (0.6667, [2])]))
        map (.onScreen) ps @?= [True, True]
    , testCase "unpark pulls a window fully back on screen" $
        unpark visible (Rect 1432 100 800 600) @?= Rect 640 100 800 600
    ]
