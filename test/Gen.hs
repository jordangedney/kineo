-- | Fixtures and QuickCheck generators shared by the specs.
module Gen
  ( laptop
  , external
  , window
  , dialog
  , genStrip
  , genEvent
  , invariant
  ) where

import Data.Foldable (toList)
import Data.List (nub)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Kineo.Command (allCommands)
import Kineo.Core
import Kineo.Geometry (Rect (..))
import Kineo.Strip (Column (..), Strip, WindowId)
import Kineo.Strip qualified as Strip
import Test.Tasty.QuickCheck

-- | A 1440x900 display with a 25px menu bar, showing space 1.
laptop :: Display
laptop =
  Display
    { displayId = 1
    , frame = Rect 0 0 1440 900
    , visibleFrame = Rect 0 25 1440 875
    , currentSpace = 1
    , userSpace = True
    }

-- | A 1920x1080 display to the right of 'laptop', showing space 2.
external :: Display
external =
  Display
    { displayId = 2
    , frame = Rect 1440 0 1920 1080
    , visibleFrame = Rect 1440 25 1920 1055
    , currentSpace = 2
    , userSpace = True
    }

-- | An ordinary tileable window on space 1.
window :: WindowId -> WindowInfo
window wid =
  WindowInfo
    { wid = wid
    , pid = 100
    , bundleId = "org.example.app"
    , title = "Document"
    , space = 1
    , bounds = Rect (fromIntegral wid * 10) 100 600 400
    , standard = True
    , resizable = True
    , movable = True
    , minimized = False
    , fullscreen = False
    }

dialog :: WindowId -> WindowInfo
dialog wid = (window wid) {standard = False}

-- | A strip whose windows are distinct.
genStrip :: Gen Strip
genStrip = do
  ids <- nub <$> listOf (chooseEnum (1, 40))
  Strip.fromColumns <$> chunk ids
  where
    chunk [] = pure []
    chunk (a : rest0) = do
      n <- chooseInt (0, 2)
      let (more, rest) = splitAt n rest0
      wd <- elements [0.3333, 0.5, 0.6667, 1]
      (Column {stack = a :| more, width = wd, savedWidth = Nothing} :) <$> chunk rest

genEvent :: Gen Event
genEvent =
  frequency
    [ (6, WindowAppeared <$> genInfo)
    , (2, WindowGone <$> wid)
    , (4, WindowFocused <$> wid)
    , (2, WindowMinimized <$> wid <*> arbitrary)
    , (1, WindowResized <$> wid <*> choose (100, 1500))
    , (1, WindowMinWidth <$> wid <*> choose (100, 3000))
    , (1, AppHidden <$> pid <*> arbitrary)
    , (1, AppTerminated <$> pid)
    , (1, Reconfigured <$> displays <*> (Map.fromList <$> listOf ((,) <$> wid <*> space)))
    , (6, Command <$> elements allCommands)
    ]
  where
    wid = chooseEnum (1, 12)
    pid = chooseEnum (1, 3)
    space = elements [0, 1, 2, 3]
    displays = elements [[laptop], [laptop, external], [external, laptop], [laptop {currentSpace = 3}, external]]
    genInfo = do
      w <- wid
      p <- pid
      s <- space
      std <- frequency [(5, pure True), (1, pure False)]
      mini <- frequency [(6, pure False), (1, pure True)]
      x <- choose (-500, 3000)
      pure (window w) {pid = p, space = s, standard = std, minimized = mini, bounds = Rect x 100 600 400}

-- | What must hold after any sequence of events.
invariant :: World -> Either String ()
invariant w = do
  let inStrips = [(sid, wid) | (sid, sp) <- Map.toList w.spaces, ws <- toList sp.workspaces, wid <- Strip.windows ws.strip]
  mapM_
    ( \(wid, t) -> do
        let homes = [sid | (sid, x) <- inStrips, x == wid]
            expected = [t.onSpace | not t.isFloating, isNothing t.tabOf]
        if homes == expected
          then pure ()
          else Left ("window " ++ show wid ++ " is in strips " ++ show homes ++ ", expected " ++ show expected)
    )
    (Map.toList w.windows)
  mapM_
    (\(_, wid) -> if Map.member wid w.windows then pure () else Left ("untracked window " ++ show wid ++ " in a strip"))
    inStrips
  mapM_
    ( \(wid, t) -> case t.tabOf of
        Just s | fmap (isNothing . (.tabOf)) (Map.lookup s w.windows) /= Just True ->
          Left ("tab " ++ show wid ++ " hides behind " ++ show s ++ ", which isn't a shown window")
        _ -> pure ()
    )
    (Map.toList w.windows)
  mapM_
    (\ws -> if ws.scroll >= 0 then pure () else Left ("negative scroll " ++ show ws.scroll))
    (concatMap (toList . (.workspaces)) (Map.elems w.spaces))
  mapM_
    ( \(sid, sp) -> do
        let n = length sp.workspaces
            emptyIdle = [i | (i, ws) <- zip [0 ..] (toList sp.workspaces), i /= sp.active, null (Strip.windows ws.strip)]
        if n >= 1 && sp.active >= 0 && sp.active < n
          then pure ()
          else Left ("space " ++ show sid ++ " has active workspace " ++ show sp.active ++ " of " ++ show n)
        if null emptyIdle
          then pure ()
          else Left ("space " ++ show sid ++ " keeps empty workspaces " ++ show emptyIdle)
    )
    (Map.toList w.spaces)
  case w.focused of
    Just f | not (Map.member f w.windows) -> Left ("focused window " ++ show f ++ " is not tracked")
    _ -> pure ()
