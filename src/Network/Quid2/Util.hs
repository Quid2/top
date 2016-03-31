{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}

module Network.Quid2.Util(
  runWSClient,protocol,sendMsg,receiveMsg
  ,dbg,warn,info,err,dbgS,logLevel
  ,liftIO,forever,when,unless,try,SomeException,threadDelay,seconds
  ,module X
  ) where

import           Control.Concurrent
import           Control.Exception      (SomeException, fromException, handle,
                                         try)
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString.Lazy   as L
import           Network.Quid2.Types
import qualified Network.WebSockets     as WS
import           System.Log.Logger      as X

-- |Run a WebSockets connection
-- Automatically close sockets on App exit
runWSClient :: Config -> WS.ClientApp a -> IO a
runWSClient cfg app = do
     WS.runClientWith (ip cfg) (port cfg) (path cfg) opts [("Sec-WebSocket-Protocol", "quid2.net")] $ \conn -> do
       WS.forkPingThread conn 20 -- Keep connection alive avoiding timeouts
       app conn
       --WS.sendClose conn (1000::Int)
   where
     opts = WS.defaultConnectionOptions -- { WS.connectionOnPong = dbgS "gotPong"}

-- |Setup a connection by sending a value specifying the routing protocol to be used
protocol :: (Model (router a), Flat (router a)) => Connection a -> router a -> IO ()
protocol (Connection conn) =  WS.sendBinaryData conn . flat . typedBytes

-- |Send a raw binary message on a WebSocket (untyped) connection
sendMsg :: WS.Connection -> L.ByteString -> IO ()
sendMsg = WS.sendBinaryData

-- |Receive a raw binary message from a WebSocket (untyped) connection
receiveMsg :: WS.Connection -> IO L.ByteString
receiveMsg conn = WS.receiveData conn

-- |Setup the global logging level
logLevel = updateGlobalLogger rootLoggerName . setLevel

seconds = (* 1000000)

-- |Utilities for logging
dbgS = debugM "quid2-net"
dbg = liftIO . dbgS . unwords
err = liftIO . errorM "quid2-net" . unwords
warn = liftIO . warningM "quid2-net" . unwords
info = liftIO . infoM "quid2-net" . unwords
