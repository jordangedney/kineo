module ConfigSpec (tests) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.IO qualified as T
import Data.Set qualified as Set
import Kineo.Command
import Kineo.Config
import Kineo.Keys
import Kineo.Layout (FocusMode (..), Params (..))
import Kineo.Strip (Dir (..))
import Test.Tasty
import Test.Tasty.HUnit

decodes :: String -> IO Config
decodes src = case decodeConfig (T.pack src) of
  Right (c, []) -> pure c
  Right (_, ws) -> assertFailure ("unexpected warnings: " ++ show ws)
  Left errs -> assertFailure (unlines errs)

chord :: String -> Chord
chord = either error id . parseChord

tests :: TestTree
tests =
  testGroup
    "Config"
    [ testCase "the example config file is exactly the defaults" $ do
        src <- T.readFile "config/kineo.toml"
        decodeConfig src @?= Right (defaultConfig, [])
    , testCase "an empty file gives the defaults" $ do
        c <- decodes ""
        c @?= defaultConfig
    , testCase "keys override defaults" $ do
        c <- decodes "[layout]\ngap = 20\nfocus = \"center\"\n"
        c.layout.gap @?= 20
        c.layout.margin @?= defaultConfig.layout.margin
        c.focusMode @?= Center
    , testCase "unknown keys are reported, not silently ignored" $
        case decodeConfig "[layout]\ngapp = 3\n" of
          Right (_, [_]) -> pure ()
          other -> assertFailure (show other)
    , testCase "bindings merge with the defaults and \"none\" unbinds" $ do
        c <- decodes "[bindings]\n\"cmd+alt+l\" = \"focus-right\"\n\"hyper+a\" = \"none\"\n"
        Map.lookup (chord "cmd+alt+l") c.bindings @?= Just (Focus DirRight)
        Map.lookup (chord "hyper+a") c.bindings @?= Nothing
        Map.lookup (chord "hyper+d") c.bindings @?= Just (Focus DirRight)
    , testCase "bad values are errors" $
        mapM_
          (\src -> either (const (pure ())) (\r -> assertFailure ("accepted " ++ show src ++ ": " ++ show r)) (decodeConfig src))
          [ "[bindings]\n\"hyper+a\" = \"fly-away\"\n"
          , "[bindings]\n\"hyper+nope\" = \"quit\"\n"
          , "[layout]\nwidths = [0.5, 0.3]\n"
          , "[layout]\ndefault-width = 1.5\n"
          , "[animation]\nfps = 0\n"
          ]
    , testCase "rules match on app and title" $ do
        c <- decodes "[[rules]]\napp = \"a.b\"\ntitle-contains = \"Scratch\"\nwidth = 0.25\n"
        fmap (.ruleWidth) (ruleFor c "a.b" "My Scratch Pad") @?= Just (Just 0.25)
        ruleFor c "a.b" "Other" @?= Nothing
    , testCase "chords" $ do
        parseChord "hyper+a" @?= Right (Chord (Set.fromList [Cmd, Alt, Ctrl]) 0x00)
        parseChord "Cmd+Shift+Left" @?= Right (Chord (Set.fromList [Cmd, Shift]) 0x7B)
        renderChord (chord "meh+f5") @?= "alt+ctrl+shift+f5"
        either (const True) (const False) (parseChord "hyper+wat") @? "unknown key accepted"
    , testCase "command names round-trip and are forgiving" $ do
        mapM_ (\c -> parseCommand (commandName c) @?= Just c) allCommands
        parseCommand "  Focus Left " @?= Just (Focus DirLeft)
        parseCommand "cycle_width_back" @?= Just CycleWidthBack
        parseCommand " Exec open -a 'My App' " @?= Just (Exec "open -a 'My App'")
        parseCommand "exec" @?= Nothing
    ]
