module CheatsheetSpec (tests) where

import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Kineo.Cheatsheet
import Kineo.Command
import Kineo.Config (defaultBindings)
import Kineo.Keys
import Kineo.Strip (Dir (..))
import Test.Tasty
import Test.Tasty.HUnit

chord :: String -> Chord
chord = either error id . parseChord

sheet :: [(String, Command)] -> String
sheet bs = cheatsheet False (Map.fromList [(chord k, c) | (k, c) <- bs])

tests :: TestTree
tests =
  testGroup
    "Cheatsheet"
    [ testCase "the four focus directions share a row" $
        assertBool "merged row" ("✦ a d w s    ← → ↑ ↓" `isInfixOf` cheatsheet False defaultBindings)
    , testCase "the defaults fit in 80 columns" $
        assertBool "too wide" (all ((<= 80) . length) (lines (cheatsheet False defaultBindings)))
    , testCase "no escape codes without colour" $
        assertBool "escape" (notElem '\ESC' (cheatsheet False defaultBindings))
    , testCase "every default binding's key is shown" $
        let s = cheatsheet False defaultBindings
         in mapM_ (\k -> assertBool k (k `isInfixOf` s)) ["✦ h", "✦ l", "✦ ⇧ q", "✦ ↩", "✦ ⌫", "✦ p", "✦ ,", "✦ ."]
    , testCase "three directions don't merge" $ do
        let s = sheet [("hyper+a", Focus DirLeft), ("hyper+d", Focus DirRight), ("hyper+w", Focus DirUp)]
        assertBool "no merged row" (not ("← → ↑ ↓" `isInfixOf` s))
        assertBool "separate rows" ("✦ a" `isInfixOf` s && "✦ w" `isInfixOf` s)
    , testCase "non-hyper chords use Mac symbols" $
        assertBool "symbols" ("⌃⌥ ↩" `isInfixOf` sheet [("ctrl+alt+return", Retile)])
    , testCase "exec names the app it opens" $ do
        assertBool "open -a" ("open Safari" `isInfixOf` sheet [("hyper+b", Exec "open -a Safari")])
        assertBool "applescript" ("open Ghostty" `isInfixOf` sheet [("hyper+return", Exec "osascript -e 'tell application \"Ghostty\" to activate'")])
    , testCase "renderKeys" $
        renderKeys (chord "hyper+shift+a") @?= "✦ ⇧ a"
    ]
