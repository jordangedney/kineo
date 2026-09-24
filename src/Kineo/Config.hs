-- | Configuration: types, defaults, and decoding from TOML. Every key is
-- optional; a missing key keeps its default.
module Kineo.Config
  ( Config (..)
  , Animation (..)
  , Easing (..)
  , Rule (..)
  , defaultConfig
  , defaultBindings
  , decodeConfig
  , ruleFor
  ) where

import Control.Monad (unless, when)
import Data.Foldable (for_)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Kineo.Command (Command (..), parseCommand)
import Kineo.Keys (Chord, parseChord)
import Kineo.Layout (FocusMode (..), Params (..))
import Kineo.Strip (Dir (..))
import Toml qualified
import Toml.Schema qualified as Toml
import Toml.Schema.Matcher qualified as Toml (inKey)

data Config = Config
  { layout :: Params
  , focusMode :: FocusMode
  , widths :: [Double]
  -- ^ Presets for 'CycleWidth', ascending fractions of the usable width.
  , defaultWidth :: Double
  , animation :: Animation
  , bindings :: Map Chord Command
  , rules :: [Rule]
  }
  deriving stock (Eq, Show)

data Animation = Animation {durationMs :: Int, fps :: Int, easing :: Easing}
  deriving stock (Eq, Show)

data Easing
  = Linear
  | EaseOut
  | EaseInOut
  | -- | A critically damped spring: keeps its speed when redirected.
    Spring
  deriving stock (Eq, Show)

-- | Per-application behaviour. A rule matches when every field it sets
-- matches; the first matching rule wins.
data Rule = Rule
  { app :: Maybe Text
  -- ^ Bundle identifier, e.g. @com.apple.finder@.
  , titleContains :: Maybe Text
  , float :: Bool
  -- ^ Leave matching windows alone instead of tiling them.
  , ruleWidth :: Maybe Double
  -- ^ Initial column width.
  }
  deriving stock (Eq, Show)

defaultConfig :: Config
defaultConfig =
  Config
    { layout = Params {gap = 12, margin = 12, sliver = 8}
    , focusMode = Reveal
    , widths = [0.3333, 0.5, 0.6667, 1]
    , defaultWidth = 1 / 2
    , animation = Animation {durationMs = 200, fps = 120, easing = Spring}
    , bindings = defaultBindings
    , rules =
        [ Rule {app = Just "com.apple.systempreferences", titleContains = Nothing, float = True, ruleWidth = Nothing}
        , Rule {app = Just "com.apple.ActivityMonitor", titleContains = Nothing, float = True, ruleWidth = Nothing}
        ]
    }

-- | A new iTerm window, using the profile named \"Kineo\" when there is
-- one (config/iterm-profile.json: no title bar). Launching iTerm opens its
-- own first window, so only a running iTerm is asked for another.
newTerminal :: String
newTerminal =
  "osascript -e 'if application \"iTerm\" is running then' -e 'tell application \"iTerm\"'"
    ++ " -e 'try' -e 'create window with profile \"Kineo\"' -e 'on error'"
    ++ " -e 'create window with default profile' -e 'end try' -e 'end tell' -e 'end if'"
    ++ " -e 'tell application \"iTerm\" to activate'"

-- | Hyper is cmd+alt+ctrl. WASD moves focus; adding shift moves the window.
defaultBindings :: Map Chord Command
defaultBindings =
  Map.fromList
    [ (chord k, c)
    | (k, c) <-
        [ ("hyper+a", Focus DirLeft)
        , ("hyper+d", Focus DirRight)
        , ("hyper+w", Focus DirUp)
        , ("hyper+s", Focus DirDown)
        , ("hyper+h", FocusFirst)
        , ("hyper+l", FocusLast)
        , ("hyper+shift+a", Move DirLeft)
        , ("hyper+shift+d", Move DirRight)
        , ("hyper+shift+w", Move DirUp)
        , ("hyper+shift+s", Move DirDown)
        , ("hyper+c", CycleWidth)
        , ("hyper+shift+c", CycleWidthBack)
        , ("hyper+f", ToggleFullWidth)
        , ("hyper+m", CenterColumn)
        , ("hyper+comma", Consume)
        , ("hyper+period", Expel)
        , ("hyper+p", ToggleFloat)
        , ("hyper+r", Retile)
        , ("hyper+shift+r", ReloadConfig)
        , ("hyper+shift+q", Quit)
        , ("hyper+return", Exec newTerminal)
        , ("hyper+delete", CloseWindow)
        ]
    ]
  where
    chord = either error id . parseChord

-- | The first rule matching a window's bundle identifier and title.
ruleFor :: Config -> Text -> Text -> Maybe Rule
ruleFor cfg bundle title = case filter matches cfg.rules of
  (r : _) -> Just r
  [] -> Nothing
  where
    matches r =
      maybe True (== bundle) r.app
        && maybe True (`T.isInfixOf` title) r.titleContains

-- | Decode a configuration file. Unknown keys come back as warnings so a
-- typo never silently does nothing.
decodeConfig :: Text -> Either [String] (Config, [String])
decodeConfig src = case Toml.decode src of
  Toml.Failure errs -> Left errs
  Toml.Success warns (FileConfig cfg) -> Right (cfg, warns)

newtype FileConfig = FileConfig Config

instance Toml.FromValue FileConfig where
  fromValue = Toml.parseTableFromValue $ do
    let d = defaultConfig
    lay <- Toml.optKeyOf "layout" (Toml.parseTableFromValue layoutTable)
    anim <- Toml.optKeyOf "animation" (Toml.parseTableFromValue animationTable)
    binds <- Toml.optKeyOf "bindings" bindingsTable
    rs <- Toml.optKeyOf "rules" (Toml.listOf (const (Toml.parseTableFromValue ruleTable)))
    let (params, mode, ws, dw) = fromMaybe (d.layout, d.focusMode, d.widths, d.defaultWidth) lay
    pure . FileConfig $
      Config
        { layout = params
        , focusMode = mode
        , widths = ws
        , defaultWidth = dw
        , animation = fromMaybe d.animation anim
        , bindings = maybe d.bindings (\b -> Map.mapMaybe id (Map.fromList b `Map.union` fmap Just d.bindings)) binds
        , rules = fromMaybe d.rules rs
        }

layoutTable :: Toml.ParseTable l (Params, FocusMode, [Double], Double)
layoutTable = do
  let d = defaultConfig
  g <- Toml.optKey "gap"
  m <- Toml.optKey "margin"
  s <- Toml.optKey "sliver"
  mode <- Toml.optKeyOf "focus" $ \v ->
    Toml.fromValue v >>= \case
      "reveal" -> pure Reveal
      "center" -> pure Center
      other -> Toml.failAt (Toml.valueAnn v) ("focus must be \"reveal\" or \"center\", not " ++ show (other :: Text))
  ws <- Toml.optKeyOf "widths" $ \v -> do
    xs <- Toml.fromValue v
    when (null xs) $ Toml.failAt (Toml.valueAnn v) "widths must not be empty"
    for_ xs (fraction v)
    unless (and (zipWith (<) xs (drop 1 xs))) $ Toml.failAt (Toml.valueAnn v) "widths must be in ascending order"
    pure xs
  dw <- Toml.optKeyOf "default-width" $ \v -> Toml.fromValue v >>= \x -> fraction v x >> pure x
  for_ [g, m, s] . traverse $ \x ->
    when (x < 0) $ Toml.warnTable "gap, margin and sliver should not be negative"
  pure
    ( Params
        { gap = fromMaybe d.layout.gap g
        , margin = fromMaybe d.layout.margin m
        , sliver = fromMaybe d.layout.sliver s
        }
    , fromMaybe d.focusMode mode
    , fromMaybe d.widths ws
    , fromMaybe d.defaultWidth dw
    )

animationTable :: Toml.ParseTable l Animation
animationTable = do
  let d = defaultConfig.animation
  dur <- Toml.optKeyOf "duration-ms" $ \v -> do
    x <- Toml.fromValue v
    when (x < 0) $ Toml.failAt (Toml.valueAnn v) "duration-ms must not be negative"
    pure x
  f <- Toml.optKeyOf "fps" $ \v -> do
    x <- Toml.fromValue v
    unless (x >= 1 && x <= 240) $ Toml.failAt (Toml.valueAnn v) "fps must be between 1 and 240"
    pure x
  e <- Toml.optKeyOf "easing" $ \v ->
    Toml.fromValue v >>= \case
      "linear" -> pure Linear
      "ease-out" -> pure EaseOut
      "ease-in-out" -> pure EaseInOut
      "spring" -> pure Spring
      other -> Toml.failAt (Toml.valueAnn v) ("unknown easing " ++ show (other :: Text))
  pure
    Animation
      { durationMs = fromMaybe d.durationMs dur
      , fps = fromMaybe d.fps f
      , easing = fromMaybe d.easing e
      }

-- | @"chord" = "command"@ pairs. The command @"none"@ removes a default.
bindingsTable :: Toml.Value' l -> Toml.Matcher l [(Chord, Maybe Command)]
bindingsTable v = do
  table <- Toml.mapOf (\_ k -> pure k) (const pure) v
  traverse entry (Map.toList table)
  where
    entry (k, cv) = Toml.inKey k $ do
      chord <- either (Toml.failAt (Toml.valueAnn cv)) pure (parseChord (T.unpack k))
      name <- Toml.fromValue cv
      case name of
        "none" -> pure (chord, Nothing)
        _ -> case parseCommand name of
          Just c -> pure (chord, Just c)
          Nothing -> Toml.failAt (Toml.valueAnn cv) ("unknown command " ++ show name)

ruleTable :: Toml.ParseTable l Rule
ruleTable = do
  a <- Toml.optKey "app"
  t <- Toml.optKey "title-contains"
  fl <- Toml.optKey "float"
  wd <- Toml.optKeyOf "width" $ \v -> Toml.fromValue v >>= \x -> fraction v x >> pure x
  pure Rule {app = a, titleContains = t, float = fromMaybe False fl, ruleWidth = wd}

fraction :: Toml.Value' l -> Double -> Toml.Matcher l ()
fraction v x =
  unless (x > 0 && x <= 1) $
    Toml.failAt (Toml.valueAnn v) ("widths are fractions of the screen and must be in (0, 1], got " ++ show x)
