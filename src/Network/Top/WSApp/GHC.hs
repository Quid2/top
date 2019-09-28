{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Top.WSApp.GHC
  ( 
#ifndef ghcjs_HOST_OS
    runWSApp
#endif
  ) where

#ifndef ghcjs_HOST_OS
import qualified Data.ByteString    as B
import           Network.Top.Types

import qualified Network.WebSockets as WS

-- |Run a WebSockets Application, keep connection alive and automatically close sockets on WSApp exit.
runWSApp ::
     Config -- ^ Top configuration
  -> WSApp a -- ^ Application to connect
  -> IO a -- ^ Value returned from the application
runWSApp cfg app =
  WS.runClientWith
    (cfgIP cfg)
    (cfgPort cfg)
    (cfgPath cfg)
    opts
    [("Sec-WebSocket-Protocol", chatsProtocol)] $ \conn
       -- WS.forkPingThread conn 20 -- Keep connection alive avoiding timeouts (FIX: the server should send pings as this is required by browsers)
       --WS.sendClose conn (1000::Int)
       -- app $ Connection
       --   (eitherToMaybe <$> tryE (WS.receiveData conn))
       --   (\bs -> isRight <$> tryE (WS.sendBinaryData conn bs))
   -> do
    app $
      Connection
        (WS.receiveData conn)
        (WS.sendBinaryData conn)
        (WS.sendClose
           conn
           ("So long, and thanks for all the fish!" :: B.ByteString))
  where
    opts = WS.defaultConnectionOptions -- { WS.connectionOnPong = dbgS "gotPong"}
-- -- |Send a raw binary message on a WebSocket (untyped) connection
-- sendMsg :: WS.Connection -> L.ByteString -> IO ()
-- sendMsg = WS.sendBinaryData
-- -- |Receive a raw binary message from a WebSocket (untyped) connection
-- receiveMsg :: WS.Connection -> IO L.ByteString
-- receiveMsg conn = WS.receiveData conn
#endif
