module StripSpec (tests) where

import Data.List (sort)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (isJust)
import Gen (genStrip)
import Kineo.Strip
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

-- | Columns written as lists of windows, all half width.
strip :: [[WindowId]] -> Strip
strip = fromColumns . map (\ws -> Column (NE.fromList ws) 0.5 Nothing)

shape :: Strip -> [[WindowId]]
shape s = [NE.toList c.stack | c <- columns s]

everyone :: WindowId -> Bool
everyone = const True

tests :: TestTree
tests =
  testGroup
    "Strip"
    [ testCase "insertAfter puts the column right of the anchor's column" $
        shape (insertAfter (Just 2) (column 0.5 9) (strip [[1], [2, 3], [4]])) @?= [[1], [2, 3], [9], [4]]
    , testCase "insertAfter without an anchor appends" $
        shape (insertAfter Nothing (column 0.5 9) (strip [[1], [2]])) @?= [[1], [2], [9]]
    , testCase "remove drops a column that becomes empty" $
        shape (remove 2 (strip [[1], [2], [3, 4]])) @?= [[1], [3, 4]]
    , testCase "sideways neighbours keep the row where they can" $ do
        let s = strip [[1, 2], [3, 4, 5], [6]]
        neighbor DirRight 2 s @?= Just 4
        neighbor DirRight 5 s @?= Just 6
        neighbor DirLeft 5 s @?= Just 2
        neighbor DirLeft 1 s @?= Nothing
        neighbor DirDown 3 s @?= Just 4
        neighbor DirUp 3 s @?= Nothing
    , testCase "moveColumn swaps with the neighbouring column" $
        shape (moveColumn everyone DirLeft 3 (strip [[1], [2], [3]])) @?= [[1], [3], [2]]
    , testCase "moveColumn steps over columns with nothing visible" $
        shape (moveColumn (/= 2) DirLeft 3 (strip [[1], [2], [3]])) @?= [[3], [1], [2]]
    , testCase "moveColumn at the edge does nothing" $
        shape (moveColumn everyone DirRight 3 (strip [[1], [2], [3]])) @?= [[1], [2], [3]]
    , testCase "moveInColumn swaps vertically" $
        shape (moveInColumn everyone DirDown 1 (strip [[1, 2, 3]])) @?= [[2, 1, 3]]
    , testCase "consume stacks a window under its left neighbour" $
        shape (consume everyone 3 (strip [[1], [2], [3, 4]])) @?= [[1], [2, 3], [4]]
    , testCase "consume of a single-window column removes that column" $
        shape (consume everyone 2 (strip [[1], [2], [3]])) @?= [[1, 2], [3]]
    , testCase "expel makes a new column to the right" $
        shape (expel 1 (strip [[1, 2], [3]])) @?= [[2], [1], [3]]
    , testCase "expel of a lone window does nothing" $
        shape (expel 3 (strip [[1, 2], [3]])) @?= [[1, 2], [3]]
    , testProperty "rearranging never loses or duplicates windows" $
        forAll genStrip $ \s ->
          let ws = windows s
           in not (null ws) ==>
                forAll (elements ws) $ \wid ->
                  forAll (elements [minBound .. maxBound]) $ \dir ->
                    conjoin
                      [ sort (windows (op wid s)) === sort ws
                      | op <- [moveColumn even dir, moveInColumn even dir, consume even, expel]
                      ]
    , testProperty "remove takes out exactly that window" $
        forAll genStrip $ \s ->
          let ws = windows s
           in not (null ws) ==>
                forAll (elements ws) $ \wid ->
                  sort (windows (remove wid s)) === sort (filter (/= wid) ws)
    , testProperty "locate finds every window" $
        forAll genStrip $ \s -> all (\wid -> isJust (locate wid s)) (windows s)
    ]
