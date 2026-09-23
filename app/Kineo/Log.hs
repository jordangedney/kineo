-- | Minimal leveled logging to stderr. The level comes from @KINEO_LOG@
-- (debug, info, warn, error; default info).
module Kineo.Log
  ( Logger
  , Level (..)
  , newLogger
  , debug
  , info
  , warn
  , err
  ) where

import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Monad (when)
import Data.Char (toLower)
import Data.Time (defaultTimeLocale, formatTime, getZonedTime)
import System.Environment (lookupEnv)
import System.IO (hFlush, hPutStrLn, stderr)

data Level = Debug | Info | Warn | Error
  deriving stock (Eq, Ord, Show)

data Logger = Logger {minLevel :: Level, lock :: MVar ()}

newLogger :: IO Logger
newLogger = do
  env <- fmap (map toLower) <$> lookupEnv "KINEO_LOG"
  let level = case env of
        Just "debug" -> Debug
        Just "warn" -> Warn
        Just "error" -> Error
        _ -> Info
  Logger level <$> newMVar ()

emit :: Level -> Logger -> String -> IO ()
emit level lg msg = when (level >= lg.minLevel) $ do
  now <- getZonedTime
  withMVar lg.lock $ \() -> do
    hPutStrLn stderr (formatTime defaultTimeLocale "%H:%M:%S%3Q" now ++ " " ++ tag ++ " " ++ msg)
    hFlush stderr
  where
    tag = case level of Debug -> "debug"; Info -> "info "; Warn -> "warn "; Error -> "error"

debug, info, warn, err :: Logger -> String -> IO ()
debug = emit Debug
info = emit Info
warn = emit Warn
err = emit Error
