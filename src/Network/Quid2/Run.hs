{-# LANGUAGE ScopedTypeVariables       #-}
module Network.Quid2.Run(
  runClient,runClientForever
  ) where

import           Data.Flat
import           Data.Typed
import           Network.Quid2.Types
import           Network.Quid2.Util

-- |Protect against crashes, restart on failure
runClientForever :: (Model (router a), Flat (router a),Show (router a)) => Config -> router a -> (Connection a -> IO b) -> IO ()
runClientForever cfg router op = forever $ do
      Left (ex :: SomeException) <- try $ runClient cfg router $ \conn -> do

        liftIO $ dbgS "connected"
        op conn

      -- Something went wrong, wait a few seconds and restart
      dbg ["Exited loop with error",concat ["'",show ex,"'"],"retrying in a bit."]
      threadDelay $ seconds 5

runClient :: (Model (router a), Flat (router a),Show (router a)) => Config -> router a -> (Connection a -> IO r) -> IO r
runClient cfg router client = runWSClient cfg (\c -> let conn = Connection c
                                                      in do
                                                   --print router
                                                   -- print $ typedBytes router
                                                   -- print $ flat $ typedBytes router
                                                   protocol conn router
                                                   client conn)

