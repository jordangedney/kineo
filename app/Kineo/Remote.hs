-- | A Unix socket for driving a running Kineo from outside: @kineo send@,
-- scripts, or a voice front end. The protocol is one command name per
-- line; each gets a one-line reply, @ok@ or @error: ...@.
module Kineo.Remote
  ( socketPath
  , serve
  , send
  , alreadyRunning
  ) where

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, bracket, try)
import Control.Monad (forever, unless, void)
import Data.ByteString.Char8 qualified as B
import Kineo.Command (Command (..), parseCommand)
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removePathForcibly)
import System.FilePath ((</>))
import System.Posix.User (getRealUserID)

-- | Fixed per user rather than under @$TMPDIR@, which differs between a
-- launchd agent, a terminal and a nix shell.
socketPath :: IO FilePath
socketPath = do
  uid <- getRealUserID
  pure ("/tmp" </> ("kineo-" ++ show uid ++ ".sock"))

-- | Is another Kineo listening on the socket?
alreadyRunning :: IO Bool
alreadyRunning = do
  path <- socketPath
  r <- try @SomeException (bracket (socket AF_UNIX Stream 0) close (\s -> connect s (SockAddrUnix path)))
  pure (either (const False) (const True) r)

-- | Accept connections forever, handing each parsed command to @run@.
serve :: (Command -> IO ()) -> IO ()
serve run = do
  path <- socketPath
  removePathForcibly path
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 8
  void . forkIO . forever $ do
    (conn, _) <- accept sock
    void . forkIO $ bracket (pure conn) close (session B.empty)
  where
    session buf conn = do
      chunk <- recv conn 1024
      unless (B.null chunk) $ do
        let (ls, rest) = splitLines (buf <> chunk)
        mapM_ (answer conn) ls
        session rest conn
    splitLines b = case B.elemIndexEnd '\n' b of
      Nothing -> ([], b)
      Just i -> (B.lines (B.take i b), B.drop (i + 1) b)
    answer conn line = case parseCommand (B.unpack line) of
      -- Running programs is for key bindings only, so the socket can't
      -- become a way to run arbitrary commands.
      Just (Exec _) -> sendAll conn "error: exec is only allowed in key bindings\n"
      Just c -> run c >> sendAll conn "ok\n"
      Nothing -> sendAll conn ("error: unknown command " <> B.pack (show (B.unpack line)) <> "\n")

-- | Send one command to the running Kineo and return its reply.
send :: String -> IO (Either String String)
send cmd = do
  path <- socketPath
  r <- try @SomeException . bracket (socket AF_UNIX Stream 0) close $ \s -> do
    connect s (SockAddrUnix path)
    sendAll s (B.pack cmd <> "\n")
    B.unpack <$> recv s 1024
  pure $ case r of
    Left _ -> Left ("Kineo is not running (no socket at " ++ path ++ ")")
    Right reply -> Right (takeWhile (/= '\n') reply)
