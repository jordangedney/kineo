module MotionSpec (tests) where

import Kineo.Config (Animation (..), Easing (..))
import Kineo.Geometry (Rect (..))
import Kineo.Motion
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

anim :: Easing -> Animation
anim e = Animation {durationMs = 200, fps = 120, easing = e}

easings :: [Easing]
easings = [Linear, EaseOut, EaseInOut, Spring]

newtype Pos = Pos Rect
  deriving stock (Show)

instance Arbitrary Pos where
  arbitrary = do
    x <- choose (-3000, 3000)
    y <- choose (-1000, 2000)
    w <- choose (100, 2000)
    h <- choose (100, 1200)
    pure (Pos (Rect x y w h))

close :: Double -> Rect -> Rect -> Bool
close eps a b = abs (a.x - b.x) < eps && abs (a.y - b.y) < eps

tests :: TestTree
tests =
  testGroup
    "Motion"
    [ testProperty "starts where the window was, at its target size" $ \(Pos a) (Pos b) ->
        conjoin
          [ let r = at (anim e) 5 (begin 5 a b) in close 1e-6 r a && r.w == b.w && r.h == b.h
          | e <- easings
          ]
    , testProperty "arrives, and stays" $ \(Pos a) (Pos b) ->
        conjoin
          [ finished (anim e) 6 m && close 0.5 (at (anim e) 6 m) b && close 0.5 (at (anim e) 9 m) b
          | e <- easings
          , let m = begin 5 a b
          ]
    , testProperty "not finished half way" $ \(Pos a) (Pos b) ->
        abs (a.x - b.x) > 50 ==> conjoin [not (finished (anim e) 5.1 (begin 5 a b)) | e <- easings]
    , testProperty "a spring from rest never overshoots" $ \(Pos a) (Pos b) (Positive t) ->
        let r = at (anim Spring) (5 + t) (begin 5 a b)
         in (r.x - b.x) * (a.x - b.x) >= 0 && (r.y - b.y) * (a.y - b.y) >= 0
    , testProperty "a spring is nearly there after the duration" $ \(Pos a) (Pos b) ->
        let r = at (anim Spring) 5.2 (begin 5 a b)
         in abs (r.x - b.x) <= 0.004 * abs (a.x - b.x) + 1e-9
    , testProperty "redirecting keeps position, and a spring's speed" $ \(Pos a) (Pos b) (Pos c) ->
        forAll (choose (0, 0.3)) $ \t ->
          conjoin
            [ close 1e-6 (at cfg now m) (at cfg now m')
                && (e /= Spring || velocity cfg now m == velocity cfg now m')
            | e <- easings
            , let cfg = anim e
                  now = 5 + t
                  m = begin 5 a b
                  m' = redirect cfg now c m
            ]
    , testCase "redirecting a moving spring keeps it moving" $ do
        let cfg = anim Spring
            m = redirect cfg 5.05 (Rect 0 0 500 500) (begin 5 (Rect 0 0 500 500) (Rect 1000 0 500 500))
        -- Heading back to 0, it first carries on past where it was.
        assertBool "overshoots" ((at cfg 5.055 m).x > (at cfg 5.05 m).x)
    , testCase "no animation goes straight there" $ do
        let cfg = (anim Spring) {durationMs = 0}
            m = begin 5 (Rect 0 0 1 1) (Rect 100 0 1 1)
        at cfg 5 m @?= Rect 100 0 1 1
        assertBool "finished" (finished cfg 5 m)
    ]
