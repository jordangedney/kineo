-- | kineo-hyper: Caps Lock becomes a hyper key (cmd+alt+ctrl) while this
-- runs. Kept apart from the window manager so restarting or crashing Kineo
-- never takes your keyboard with it.
module Main (main) where

import Control.Concurrent (threadDelay)
import Control.Monad (forM_, forever, unless)
import Foreign.C.Types (CBool (..))
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.Posix.Process (exitImmediately)
import System.Posix.Signals (Handler (..), installHandler, sigHUP, sigINT, sigTERM)

foreign import ccall safe "kh_trusted" kh_trusted :: CBool -> IO CBool
foreign import ccall safe "kh_configure" kh_configure :: CBool -> CBool -> IO CBool

main :: IO ()
main = do
  args <- getArgs
  escape <- case args of
    [] -> pure False
    ["--escape"] -> pure True
    _ -> do
      putStr usage
      if args `elem` [["--help"], ["-h"]] then exitSuccess else exitFailure

  trusted <- (/= 0) <$> kh_trusted 1
  unless trusted $ do
    say "kineo-hyper needs Accessibility access: System Settings > Privacy & Security > Accessibility."
    say "Waiting for permission..."
    let wait = kh_trusted 0 >>= \ok -> unless (ok /= 0) (threadDelay 1000000 >> wait)
    wait

  -- Handlers go in before the mapping, so there is no moment where a kill
  -- would leave Caps Lock remapped. Restoring before anything was installed
  -- is a no-op.
  forM_ [sigINT, sigTERM, sigHUP] $ \s ->
    installHandler s (Catch (kh_configure 0 0 >> exitImmediately ExitSuccess)) Nothing
  ok <- kh_configure 1 (if escape then 1 else 0)
  unless (ok /= 0) $ say "could not remap Caps Lock or create the keyboard event tap" >> exitFailure

  say ("Caps Lock is hyper (cmd+alt+ctrl)" ++ (if escape then "; tap it alone for Escape" else ""))
  forever (threadDelay maxBound)

say :: String -> IO ()
say = hPutStrLn stderr . ("kineo-hyper: " ++)

usage :: String
usage =
  unlines
    [ "kineo-hyper: make Caps Lock a hyper key (cmd+alt+ctrl)"
    , ""
    , "  kineo-hyper            hold Caps Lock for hyper"
    , "  kineo-hyper --escape   ...and tap it alone for Escape"
    , ""
    , "Caps Lock goes back to normal when kineo-hyper exits."
    ]
