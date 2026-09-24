-- | A cheatsheet of the key bindings, printed when Kineo starts in a
-- terminal. Directions bound on the same modifiers share a row, and the
-- sections are laid out in two columns.
module Kineo.Cheatsheet
  ( cheatsheet
  , renderKeys
  ) where

import Data.Char (isSpace)
import Data.List (dropWhileEnd, elemIndex, isInfixOf, nub, sortOn, stripPrefix)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Word (Word32)
import Kineo.Command (Command (..), allCommands)
import Kineo.Keys (Chord (..), Modifier (..))
import Kineo.Keys qualified as Keys
import Kineo.Strip (Dir (..))

-- | The whole sheet, ending in a newline. With @colour@, headings are bold
-- and keys highlighted with ANSI escapes.
cheatsheet :: Bool -> Map Chord Command -> String
cheatsheet colour binds = unlines ([""] ++ map indent (legend : "" : columns) ++ [""])
  where
    sections =
      [ (title, rs)
      | (title, belongs) <- groups
      , let rs = rows (sortOn (rank . snd) [(c, cmd) | (c, cmd) <- Map.toList binds, belongs cmd])
      , not (null rs)
      ]
    keyW = maximum (0 : [length k | (_, rs) <- sections, (k, _) <- rs])
    blockW = maximum (0 : [keyW + 2 + length d | (_, rs) <- sections, (_, d) <- rs])
    -- A block is a section's lines, each with its width without escapes.
    blocks =
      [ (length title, bold title) : [(keyW + 2 + length d, key k ++ spaces (keyW - length k + 2) ++ d) | (k, d) <- rs]
      | (title, rs) <- sections
      ]
    (left, right) = balance blocks
    columns =
      map (dropWhileEnd isSpace) $
        zipLong
          (\(w, l) (_, r) -> l ++ spaces (blockW + 4 - w) ++ r)
          (0, "")
          (joinBlocks left)
          (joinBlocks right)
    joinBlocks = drop 1 . concatMap ((0, "") :)

    legend =
      bold "Kineo"
        ++ concat ["   " ++ key "✦" ++ " hyper (cmd+alt+ctrl)" | any isHyper (Map.keys binds)]
        ++ concat ["   " ++ key "⇧" ++ " shift" | any (Set.member Shift . (.modifiers)) (Map.keys binds)]
    indent l = if null l then l else "  " ++ l
    bold = styled "1"
    key = styled "36"
    styled code s = if colour then "\ESC[" ++ code ++ "m" ++ s ++ "\ESC[0m" else s
    spaces n = replicate n ' '

groups :: [(String, Command -> Bool)]
groups =
  [ ("Focus", \case Focus _ -> True; FocusFirst -> True; FocusLast -> True; _ -> False)
  , ("Move", \case Move _ -> True; _ -> False)
  , ("Width", \case CycleWidth -> True; CycleWidthBack -> True; ToggleFullWidth -> True; CenterColumn -> True; _ -> False)
  , ("Stack", \case Consume -> True; Expel -> True; _ -> False)
  , ("Windows", \case ToggleFloat -> True; CloseWindow -> True; Exec _ -> True; _ -> False)
  , ("Kineo", \case Retile -> True; ReloadConfig -> True; Quit -> True; _ -> False)
  ]

-- | Commands in the order 'allCommands' lists them; @exec@ last.
rank :: Command -> Int
rank cmd = fromMaybe (length allCommands) (elemIndex cmd allCommands)

data Family = FocusDir | MoveDir
  deriving stock (Eq)

family :: Command -> Maybe (Family, Dir)
family = \case
  Focus d -> Just (FocusDir, d)
  Move d -> Just (MoveDir, d)
  _ -> Nothing

-- | Rows of (keys, description). When each direction of focus (or move) has
-- exactly one binding on the same modifiers, the four share a row:
-- @✦ a d w s  ← → ↑ ↓@.
rows :: [(Chord, Command)] -> [(String, String)]
rows binds =
  [(unwords (renderMods mods : map keyName ks), "← → ↑ ↓") | (_, mods, ks) <- merged]
    ++ [(renderKeys c, describe cmd) | (c, cmd) <- binds, not (isMerged c cmd)]
  where
    dirBinds = [(f, c.modifiers, d, c.key) | (c, cmd) <- binds, Just (f, d) <- [family cmd]]
    merged =
      [ (f, mods, ks)
      | (f, mods) <- nub [(f, mods) | (f, mods, _, _) <- dirBinds]
      , Just ks <- [traverse (\d -> one [k | (f', m, d', k) <- dirBinds, f' == f, m == mods, d' == d]) [minBound .. maxBound]]
      ]
    isMerged c cmd = case family cmd of
      Just (f, _) -> any (\(f', mods, _) -> f' == f && mods == c.modifiers) merged
      Nothing -> False
    one = \case [k] -> Just k; _ -> Nothing

describe :: Command -> String
describe = \case
  Focus d -> arrow d
  FocusFirst -> "first"
  FocusLast -> "last"
  Move d -> arrow d
  CycleWidth -> "cycle"
  CycleWidthBack -> "cycle back"
  ToggleFullWidth -> "full width"
  CenterColumn -> "centre"
  Consume -> "into the left column"
  Expel -> "out of the stack"
  ToggleFloat -> "float / tile"
  CloseWindow -> "close"
  Retile -> "retile"
  ReloadConfig -> "reload config"
  Quit -> "quit"
  Exec s -> maybe (shorten s) ("open " ++) (appName s)
  where
    arrow = \case DirLeft -> "←"; DirRight -> "→"; DirUp -> "↑"; DirDown -> "↓"
    shorten s = if length s > 24 then take 23 s ++ "…" else s

-- | The application an @exec@ command opens, if it plainly says:
-- @open -a Safari@ or AppleScript's @application \"Ghostty\"@.
appName :: String -> Maybe String
appName s
  | Just rest <- stripPrefix "open -a " s = Just (takeWhile (not . isSpace) rest)
  | "application \"" `isInfixOf` s = Just (takeWhile (/= '"') (after "application \"" s))
  | otherwise = Nothing
  where
    after pat str = case stripPrefix pat str of
      Just rest -> rest
      Nothing -> case str of [] -> []; _ : xs -> after pat xs

-- | A chord in Mac symbols, with ✦ for hyper: @✦ ⇧ a@, @⌃⌥ ↩@.
renderKeys :: Chord -> String
renderKeys c = unwords [renderMods c.modifiers, keyName c.key]

renderMods :: Set Modifier -> String
renderMods mods
  | Set.fromList [Cmd, Alt, Ctrl] `Set.isSubsetOf` mods = unwords ("✦" : ["⇧" | Set.member Shift mods])
  | otherwise = concat [sym | (m, sym) <- [(Ctrl, "⌃"), (Alt, "⌥"), (Shift, "⇧"), (Cmd, "⌘")], Set.member m mods]

-- | Mac symbols for keys that have one.
keyName :: Word32 -> String
keyName k = case Keys.keyName k of
  "return" -> "↩"
  "delete" -> "⌫"
  "tab" -> "⇥"
  "escape" -> "⎋"
  "left" -> "←"
  "right" -> "→"
  "up" -> "↑"
  "down" -> "↓"
  "comma" -> ","
  "period" -> "."
  name -> name

isHyper :: Chord -> Bool
isHyper c = Set.fromList [Cmd, Alt, Ctrl] `Set.isSubsetOf` c.modifiers

-- | Split blocks into two columns of about equal height, in order.
balance :: [[a]] -> ([[a]], [[a]])
balance bs = go 0 [] bs
  where
    total = sum (map ((+ 1) . length) bs)
    go _ acc [] = (reverse acc, [])
    go h acc (b : rest)
      | h > 0 && 2 * (h + length b + 1) > total + length b + 1 = (reverse acc, b : rest)
      | otherwise = go (h + length b + 1) (b : acc) rest

zipLong :: (a -> a -> b) -> a -> [a] -> [a] -> [b]
zipLong f z = go
  where
    go (x : xs) (y : ys) = f x y : go xs ys
    go (x : xs) [] = f x z : go xs []
    go [] (y : ys) = f z y : go [] ys
    go [] [] = []
