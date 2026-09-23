module Main (main) where

import Control.Monad (forM_, when)
import Data.List (intercalate)
import Data.Text qualified as T
import Data.Version (showVersion)
import Kineo.Command (allCommands, commandName)
import Kineo.Config (Rule (..), ruleFor)
import Kineo.Core (Display (..), WindowInfo (..), tileable)
import Kineo.Geometry (Rect (..))
import Kineo.Log qualified as Log
import Kineo.Platform qualified as Platform
import Kineo.Remote qualified as Remote
import Kineo.Runtime (defaultConfigPath, loadConfig, run)
import Numeric (showFFloat)
import Paths_kineo (version)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runWith Nothing
    ["--config", path] -> runWith (Just path)
    ("send" : ws@(_ : _)) -> Remote.send (unwords ws) >>= either die' putStrLn'
    ["commands"] -> mapM_ (putStrLn . commandName) allCommands
    ["check-config"] -> defaultConfigPath >>= checkConfig
    ["check-config", path] -> checkConfig path
    ["doctor"] -> doctor
    ["--version"] -> putStrLn ("kineo " ++ showVersion version)
    _ -> putStr usage >> when (args /= ["--help"] && args /= ["-h"]) exitFailure
  where
    runWith path = do
      lg <- Log.newLogger
      maybe defaultConfigPath pure path >>= run lg
    putStrLn' reply = putStrLn reply >> when (take 5 reply == "error") exitFailure
    die' msg = hPutStrLn stderr msg >> exitFailure

usage :: String
usage =
  unlines
    [ "kineo: a scrolling tiling window manager for macOS"
    , ""
    , "  kineo [--config FILE]   run the window manager"
    , "  kineo send COMMAND      tell the running window manager to do something"
    , "  kineo commands          list every command"
    , "  kineo check-config [FILE]"
    , "  kineo doctor            show permissions, displays and windows"
    , ""
    , "The config file defaults to ~/.config/kineo/kineo.toml."
    , "Set KINEO_LOG=debug for detailed logs."
    ]

checkConfig :: FilePath -> IO ()
checkConfig path =
  loadConfig path >>= \case
    Left errs -> mapM_ (hPutStrLn stderr) errs >> exitFailure
    Right (_, warns) -> do
      mapM_ (hPutStrLn stderr . ("warning: " ++)) warns
      putStrLn (path ++ ": ok")

-- | Everything Kineo can see, without changing anything.
doctor :: IO ()
doctor = do
  Platform.initialise
  trusted <- Platform.accessibilityTrusted False
  path <- defaultConfigPath
  cfg <- either (const Nothing) (Just . fst) <$> loadConfig path
  running <- Remote.alreadyRunning

  putStrLn ("accessibility  " ++ if trusted then "granted" else "NOT granted (System Settings > Privacy & Security > Accessibility)")
  putStrLn ("config         " ++ path ++ maybe " (invalid: run kineo check-config)" (const "") cfg)
  putStrLn ("running        " ++ if running then "yes" else "no")

  ds <- Platform.displays
  putStrLn ("\ndisplays (" ++ show (length ds) ++ ")")
  forM_ ds $ \d ->
    putStrLn $
      "  " ++ show d.displayId ++ "  " ++ rect d.frame ++ "  usable " ++ rect d.visibleFrame
        ++ "  space " ++ show d.currentSpace ++ (if d.userSpace then "" else " (full screen)")

  when trusted $ do
    wids <- Platform.scanWindows
    infos <- concat <$> mapM (fmap (maybe [] pure) . Platform.queryWindow) wids
    putStrLn ("\nwindows (" ++ show (length infos) ++ ")")
    forM_ infos $ \i -> do
      let verdict
            | not (tileable i) = "ignored (" ++ intercalate ", " (reasons i) ++ ")"
            | Just r <- cfg >>= \c -> ruleFor c i.bundleId i.title, r.float = "floats (rule)"
            | otherwise = "tiled"
      putStrLn $
        "  " ++ pad 8 (show i.wid) ++ pad 36 (T.unpack i.bundleId) ++ pad 22 verdict
          ++ "space " ++ pad 6 (show i.space) ++ T.unpack (T.take 40 i.title)
  where
    rect r = show (round r.w :: Int) ++ "x" ++ show (round r.h :: Int) ++ " at " ++ num r.x ++ "," ++ num r.y
    num v = showFFloat (Just 0) v ""
    pad n s = take n (s ++ replicate n ' ') ++ " "
    reasons i =
      [ "not a standard window" | not i.standard ]
        ++ ["not resizable" | not i.resizable]
        ++ ["not movable" | not i.movable]
        ++ ["full screen" | i.fullscreen]
