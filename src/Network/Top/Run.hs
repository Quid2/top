{-# LANGUAGE ScopedTypeVariables #-}
module Network.Top.Run(
  runClient,runClientForever,protocol
            --,send,receive
  ) where

import qualified Data.ByteString.Lazy     as L
import           Data.Flat
import           Data.Typed
import           Network.Top.Types
import           Network.Top.Util
import           Network.Top.WebSockets
import qualified Data.Text                         as T

-- |Permanently connect an application to a typed channel.
-- |Restart client in case of network or application failure.
-- |BUG: no way to preserve client's state
runClientForever :: (Model (router a), Flat (router a),Show (router a),Flat a,Show a) => Config -> router a -> App a r -> IO ()
runClientForever cfg router app = forever $ do
      Left (ex :: SomeException) <- try $ runClient cfg router $ \conn -> do
        liftIO $ dbgS "connected"
        app conn

      -- Something went wrong, wait a few seconds and restart
      dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
      threadDelay $ seconds 5

-- |Connect an application to a typed channel.
runClient :: (Model (router a), Flat (router a),Show (router a),Flat a,Show a) => Config -> router a -> App a r -> IO r
runClient cfg router app = do
      dbg ["run",show router]
      run cfg 1
  where
    run _   4 = errIn "Too many redirects"
    run cfg n = do
      res <- runWSClient cfg $ \conn -> do
        send conn . typedBytes $ router
        r::WSChannelResult <- receive conn
        case r of
          Failure why -> errIn (T.unpack why)
          Success -> Right <$> app (Connection (receive conn) (send conn) (close conn))
          RetryAt addr -> return (Left $ Config addr)
      case res of
        Left cfg' -> run cfg' (n+1)
        Right r -> return r
    errIn msg = dbgS msg >> error msg

-- |Send a value on a typed connection
send :: (Show a,Flat a) => WSConnection -> a -> IO ()
send conn v = do
  let e = flat v
  output conn e
  --dbg ["sent",show v,"as",show $ L.unpack e]

-- |Receive a value from a typed connection
-- receive :: Flat a => WSConnection -> IO (Maybe a)
-- receive conn = do
--   mbs <- input conn
--   return $ case mbs of
--     Nothing -> Nothing
--     Just bs -> eitherToMaybe $ unflat bs
receive :: (Show a,Flat a) => WSConnection -> IO a
receive conn = do
  e <- input conn
  let es = show $ L.unpack e
  case unflat e of
    Left err -> dbg ["received wrong data",es] >> error "received wrong data"
    --Right v -> dbg ["received",show v,"as",es] >> return v
    Right v -> return v

-- |Setup a connection by sending a value specifying the routing protocol to be used
-- protocol :: (Model (router a), Flat (router a)) => Connection a -> router a -> IO Bool
-- protocol conn =  send conn . typedBytes
protocol :: (Show r, Model r, Flat r) => Connection ByteString -> r -> IO ()
protocol conn router = do
  dbg ["protocol",show router]
  send conn . typedBytes $ router
  r::WSChannelResult <- receive conn
  case r of
    Failure why -> err $ T.unpack why
    Success -> return ()
    RetryAt addr -> err (show $ RetryAt addr)
   where
     err msg = error msg -- $ unwords ["Failed to establish connection:",msg]

