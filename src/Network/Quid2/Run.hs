{-# LANGUAGE ScopedTypeVariables #-}
module Network.Quid2.Run(
  runClient,runClientForever
            --,send,receive
  ) where

import qualified Data.ByteString.Lazy     as L
import           Data.Flat
import           Data.Typed
import           Network.Quid2.Types
import           Network.Quid2.Util
import           Network.Quid2.WebSockets

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
runClient cfg router app
  = runWSClient cfg (\conn ->
                      do
                        dbgS "[Protocol"
                        protocol conn router
                        dbgS "Protocol]"
                        app $ Connection (receive conn) (send conn) (close conn))

  where
    -- |Send a value on a typed connection
    send :: (Show a,Flat a) => WSConnection -> a -> IO ()
    send conn v = do
      let e = flat v
      output conn e
      dbg ["sent",show v,"as",show $ L.unpack e]

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
        Right v -> dbg ["received",show v,"as",es] >> return v


