{-# LANGUAGE ScopedTypeVariables       #-}
module Network.Quid2.Run(
  runClient,runClientForever,send,receive
  ) where

import           Data.Flat
import           Data.Typed
import           Network.Quid2.Types
import           Network.Quid2.Util

-- |Permanently connect an application to a typed channel.
-- |Restart in case of network or application failure.
runClientForever :: (Model (router a), Flat (router a),Show (router a)) => Config -> router a -> App a r -> IO ()
runClientForever cfg router app = forever $ do
      Left (ex :: SomeException) <- try $ runClient cfg router $ \conn -> do

        liftIO $ dbgS "connected"
        app conn

      -- Something went wrong, wait a few seconds and restart
      dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
      threadDelay $ seconds 5

 -- |Connect an application to a typed channel.
runClient :: (Model (router a), Flat (router a),Show (router a)) => Config -> router a -> App a r -> IO r
runClient cfg router app = runWSClient cfg (\c -> let conn = Connection c
                                                  in do
                                               --print router
                                               -- print $ typedBytes router
                                               -- print $ flat $ typedBytes router
                                               protocol conn router
                                               app conn)

-- |Send a value on a typed connection
send :: (Show a,Flat a) => Connection a -> a -> IO ()
send (Connection conn) v = do
   sendMsg conn $ flat v
   dbg ["sent",show v]

-- |Receive a value from a typed connection
receive :: (Show a, Flat a) => Connection a -> IO a
receive (Connection conn) = do
    Right v <- unflat <$> receiveMsg conn
    dbg ["received",show v]
    return v

