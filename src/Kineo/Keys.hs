-- | Keyboard chords such as @hyper+shift+a@ and their macOS virtual key
-- codes. Key codes are positional (ANSI layout), so bindings stay on the
-- same physical keys whatever the input source.
module Kineo.Keys
  ( Modifier (..)
  , Chord (..)
  , parseChord
  , renderChord
  , keyName
  , carbonModifiers
  , keyCode
  ) where

import Data.Bits ((.|.))
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word32)

data Modifier = Cmd | Alt | Ctrl | Shift
  deriving stock (Eq, Ord, Show, Enum, Bounded)

data Chord = Chord {modifiers :: Set Modifier, key :: Word32}
  deriving stock (Eq, Ord, Show)

-- | Parse @mod+mod+key@. Modifier names: @cmd@, @alt@ (@opt@, @option@),
-- @ctrl@ (@control@), @shift@, plus the shorthands @hyper@ (cmd+alt+ctrl)
-- and @meh@ (ctrl+alt+shift).
parseChord :: String -> Either String Chord
parseChord s = case reverse (splitPlus (map toLower s)) of
  [] -> Left "empty key binding"
  (k : ms) -> do
    code <- maybe (Left ("unknown key " ++ show k ++ " in " ++ show s)) Right (keyCode k)
    mods <- traverse modifier ms
    pure Chord {modifiers = Set.unions mods, key = code}
  where
    modifier m = case m of
      "cmd" -> Right (Set.singleton Cmd)
      "command" -> Right (Set.singleton Cmd)
      "alt" -> Right (Set.singleton Alt)
      "opt" -> Right (Set.singleton Alt)
      "option" -> Right (Set.singleton Alt)
      "ctrl" -> Right (Set.singleton Ctrl)
      "control" -> Right (Set.singleton Ctrl)
      "shift" -> Right (Set.singleton Shift)
      "hyper" -> Right (Set.fromList [Cmd, Alt, Ctrl])
      "meh" -> Right (Set.fromList [Ctrl, Alt, Shift])
      _ -> Left ("unknown modifier " ++ show m ++ " in " ++ show s)

-- | Split on @+@, treating a trailing @+@ as part of the key is not
-- supported: bind @equal@ instead.
splitPlus :: String -> [String]
splitPlus str = case break (== '+') str of
  (a, []) -> [a]
  (a, _ : rest) -> a : splitPlus rest

renderChord :: Chord -> String
renderChord c = intercalate "+" (map name (Set.toList c.modifiers) ++ [keyName c.key])
  where
    name = \case Cmd -> "cmd"; Alt -> "alt"; Ctrl -> "ctrl"; Shift -> "shift"

-- | A key code's canonical name, as 'parseChord' reads it.
keyName :: Word32 -> String
keyName code = case [k | (k, v) <- keyTable, v == code] of
  (k : _) -> k
  [] -> show code

-- | The modifier mask Carbon's @RegisterEventHotKey@ expects.
carbonModifiers :: Set Modifier -> Word32
carbonModifiers = foldr ((.|.) . bit) 0
  where
    bit = \case Cmd -> 0x100; Shift -> 0x200; Alt -> 0x800; Ctrl -> 0x1000

keyCode :: String -> Maybe Word32
keyCode k = lookup k keyTable

-- | Names are listed canonical-first so 'renderChord' picks the nicest one.
keyTable :: [(String, Word32)]
keyTable =
  zip (map pure ['a' .. 'z']) letters
    ++ zip (map show [0 .. 9 :: Int]) [0x1D, 0x12, 0x13, 0x14, 0x15, 0x17, 0x16, 0x1A, 0x1C, 0x19]
    ++ [ ("minus", 0x1B), ("-", 0x1B)
       , ("equal", 0x18), ("=", 0x18)
       , ("leftbracket", 0x21), ("[", 0x21)
       , ("rightbracket", 0x1E), ("]", 0x1E)
       , ("semicolon", 0x29), (";", 0x29)
       , ("quote", 0x27), ("'", 0x27)
       , ("comma", 0x2B), (",", 0x2B)
       , ("period", 0x2F), (".", 0x2F)
       , ("slash", 0x2C), ("/", 0x2C)
       , ("backslash", 0x2A), ("\\", 0x2A)
       , ("grave", 0x32), ("`", 0x32)
       , ("return", 0x24), ("enter", 0x24)
       , ("tab", 0x30)
       , ("space", 0x31)
       , ("delete", 0x33), ("backspace", 0x33)
       , ("escape", 0x35), ("esc", 0x35)
       , ("left", 0x7B), ("right", 0x7C), ("down", 0x7D), ("up", 0x7E)
       , ("home", 0x73), ("end", 0x77), ("pageup", 0x74), ("pagedown", 0x79)
       ]
    ++ zip [ 'f' : show n | n <- [1 .. 12 :: Int]]
      [0x7A, 0x78, 0x63, 0x76, 0x60, 0x61, 0x62, 0x64, 0x65, 0x6D, 0x67, 0x6F]
  where
    letters =
      [ 0x00, 0x0B, 0x08, 0x02, 0x0E, 0x03, 0x05, 0x04, 0x22, 0x26, 0x28, 0x25, 0x2E
      , 0x2D, 0x1F, 0x23, 0x0C, 0x0F, 0x01, 0x11, 0x20, 0x09, 0x0D, 0x07, 0x10, 0x06
      ]
