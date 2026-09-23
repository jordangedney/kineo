-- | Everything a user can ask the window manager to do. Commands have stable
-- kebab-case names used by key bindings, @kineo send@, and anything else
-- that drives Kineo from outside (a voice front end, scripts).
module Kineo.Command
  ( Command (..)
  , commandName
  , parseCommand
  , allCommands
  ) where

import Data.Char (isSpace, toLower)
import Kineo.Strip (Dir (..))

data Command
  = Focus Dir
  | FocusFirst
  | FocusLast
  | Move Dir
  | CycleWidth
  | CycleWidthBack
  | ToggleFullWidth
  | CenterColumn
  | Consume
  | Expel
  | ToggleFloat
  | CloseWindow
  | Retile
  | ReloadConfig
  | Quit
  | -- | Run a shell command, such as opening a terminal. Written
    -- @exec \<command\>@; accepted from key bindings but not from the socket.
    Exec String
  deriving stock (Eq, Show)

allCommands :: [Command]
allCommands =
  map Focus dirs
    ++ [FocusFirst, FocusLast]
    ++ map Move dirs
    ++ [CycleWidth, CycleWidthBack, ToggleFullWidth, CenterColumn, Consume, Expel, ToggleFloat, CloseWindow, Retile, ReloadConfig, Quit]
  where
    dirs = [minBound .. maxBound]

commandName :: Command -> String
commandName = \case
  Focus d -> "focus-" ++ dirName d
  FocusFirst -> "focus-first"
  FocusLast -> "focus-last"
  Move d -> "move-" ++ dirName d
  CycleWidth -> "cycle-width"
  CycleWidthBack -> "cycle-width-back"
  ToggleFullWidth -> "toggle-full-width"
  CenterColumn -> "center"
  Consume -> "consume"
  Expel -> "expel"
  ToggleFloat -> "toggle-float"
  CloseWindow -> "close"
  Retile -> "retile"
  ReloadConfig -> "reload-config"
  Quit -> "quit"
  Exec s -> "exec " ++ s
  where
    dirName = \case DirLeft -> "left"; DirRight -> "right"; DirUp -> "up"; DirDown -> "down"

-- | Case-insensitive; spaces and underscores count as dashes, so
-- @"Focus Left"@ parses too. @exec@ keeps the rest of the line as written.
parseCommand :: String -> Maybe Command
parseCommand s = case break isSpace (trim s) of
  (verb, rest) | map toLower verb == "exec", not (null (trim rest)) -> Just (Exec (trim rest))
  _ -> lookup (normalise s) [(commandName c, c) | c <- allCommands]
  where
    normalise = map dash . map toLower . trim
    dash c = if c == ' ' || c == '_' then '-' else c
    trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
