{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnboxedTuples, GHCForeignImportPrim, UnliftedFFITypes,
              MagicHash
 #-}
module Network.Top.WebSockets(
  runWSClient--,sendMsg,receiveMsg
  ,protocol
  ) where

import qualified Data.ByteString     as B
import qualified Data.ByteString.Lazy     as L
import           Network.Top.Types
import Network.Top.Util
import Control.Exception

#ifdef ghcjs_HOST_OS
-- GHC-JS Version
import JavaScript.Web.WebSocket hiding (close)
import qualified JavaScript.Web.WebSocket as JS
import qualified Data.JSString              as S
import GHCJS.Types
import GHCJS.Marshal
import GHCJS.Marshal.Pure
import GHCJS.Buffer
import qualified GHCJS.Buffer as Buffer
import GHCJS.Foreign.Internal
import JavaScript.Web.MessageEvent
import qualified Control.Concurrent.STM as S
import Control.Applicative (Alternative(empty, (<|>)))
import Data.Maybe
import JavaScript.Web.Blob.Internal (Blob, SomeBlob(..))
-- import JavaScript.TypedArray -- ArrayBuffer, SomeArrayBuffer(..))
import JavaScript.TypedArray.ArrayBuffer -- ArrayBuffer, SomeArrayBuffer(..))
-- import JavaScript.TypedArray.Internal -- ArrayBuffer, SomeArrayBuffer(..))
import GHC.Exts

data ConnStatus a = ConnOpening
                  | ConnOpen {inp::IO a,out::a -> IO (),cls::IO ()}
                  | ConnClosed -- is this needed?

data Conn a = Conn {connConfig:: Config
                   ,connStatus :: S.TVar (ConnStatus a)
                   ,connMessages :: S.TQueue a}

runWSClient :: Config -> WSApp r -> IO r
runWSClient cfg = bracket (newConnection cfg) close

newConnection cfg = do
  conn <- Conn cfg <$> S.newTVarIO ConnClosed <*> S.newTQueueIO

  -- BUG: reopen connection without sending the protocol
  -- return $ Connection
  --    (open conn >>= \(i,o,c) -> i)
  --    (\v -> open conn >>= \(i,o,c) -> o v)
  --    (tillClose $ connStatus conn)
  (i,o,c) <- open conn
  return $ Connection i o (tillClose $ connStatus conn)

  where
   tillClose st = do
     dbgS "[Close"
     toClose <- S.atomically $ do
       s <- S.readTVar st
       case s of
         ConnOpening -> S.retry
         ConnOpen _ _ cls -> return $ Just cls
         ConnClosed -> return Nothing

     fromMaybe (return ()) toClose
     dbgS "Close]"

-- changeStatus conn = S.atomically $ S.writeTVar (connStatus conn) ConnClosed

-- |Block till open
open c =
  fromMaybe <$> reopen c <*> (S.atomically $ do
    s <- S.readTVar (connStatus c)
    case s of
      ConnOpening -> S.retry
      ConnOpen i o c -> return $ Just (i,o,c)
      ConnClosed -> do
         S.writeTVar (connStatus c) ConnOpening
         return Nothing)

reopen c = do
  dbgS "[reopen"
  let cfg = connConfig c
  econn <- tryE (JS.connect $ JS.WebSocketRequest {
                    url= S.pack $ concat ["ws://",ip cfg,":",show (port cfg),path cfg]
                    ,protocols=[S.pack "quid2.net"]
                    ,onClose  =  Just $ \_ -> closeConn c
                    ,onMessage = Just $ \event -> do
                        dbgS "received message"
                        case getDataFixed event of
                          Left e -> error e
                          Right bs -> S.atomically . S.writeTQueue (connMessages c) . L.fromStrict $ bs
                        -- case getDataFixed event of
                        --   StringData s -> dbgS "unexpected String message"
                        --   BlobData blob -> dbgS "unexpected Blob message"
                        --   ArrayBufferData ab -> S.atomically . S.writeTQueue (connMessages c) . L.fromStrict . toBS $ ab
                        })

  case econn of
    Left e -> do
      dbgS "Error while opening connection"
      threadDelay (seconds 5)
      reopen c

    Right ws -> do
          let
            -- if there is data return it,
            -- otherwise if connection is open retry otherwise return Nothing
            -- inp = do
            --  mmsg <- S.atomically $
            --     ((Just <$> S.readTQueue (connMessages c))
            --                   <|> (Do
            --                           connClosed <- isClosed <$> S.readTVar (connStatus c)
            --                           case st of
            --                             ConnOpening -> retry
            --                             ConnOpen -> retry
            --                             ConnClosed -> return Nothing
            --                           S.check connClosed
            --                           return Nothing
            --                       ))
            --  case mmsg of
            --    Nothing -> open c >> mmsg
            --    Just msg -> return msg
             inp = do
               r <- S.atomically $ S.readTQueue (connMessages c)
               dbg ["received",show $ L.unpack r]
               return r

             out v = dbg ["send",show $ L.unpack v] >> webSocketSend ws (L.toStrict v)
             -- out v = do
              --   r <- tryE (webSocketSend conn (L.toStrict v))
             --   case r of
             --     Left err -> do
             --       cls (unwords ["websockets output failed",show err])
             --       (_,o,_) <- reopen c
             --       o v

             --     Right () -> return ()

             cls reason = do
               -- closeConn conn reason
               JS.close Nothing Nothing ws -- (S.pack "FIX THIS") ws
               dbg [reason,"closed]"]

          let cls' = (cls "user request")
          js_asArrayBuffer ws
          S.atomically $ S.writeTVar (connStatus c) (ConnOpen inp out cls')
          dbgS "reopen]"
          return (inp,out,cls')
  where
     closeConn conn = S.atomically $ S.writeTVar (connStatus conn) ConnClosed

-- closeConn conn reason = do
--    JS.close conn
--    dbg ["Closing connection",reason]

-- runWSClient :: Config -> WSApp r -> IO r
-- runWSClient cfg app = do
--         q <- S.newTQueueIO
--         closed <- S.newTVarIO False

--         let wsClose _ = close "WebSockets Close"

--             close reason = do
--               S.atomically (S.writeTVar closed True)
--               dbg ["connection closed",reason]

--             wsMessage e = do
--               dbgS "received message"
--               case getData e of
--                 StringData s -> close "unexpected String message"
--                 BlobData blob -> close "unexpected Blob message"
 --                 ArrayBufferData ab -> S.atomically . S.writeTQueue q . L.fromStrict . toBS $ ab
  -- ArrayBufferData ab -> Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer $ ab
--         Conn <- JS.connect $ JS.WebSocketRequest {
--           url= S.pack $ concat ["ws://",ip cfg,":",show (port cfg),path cfg]
--           ,protocols=[S.pack "quid2.net"]
--           ,onClose=Just wsClose
--           ,onMessage=Just wsMessage}

--         let
--              out v = do
--                r <- tryE (webSocketSend conn (L.toStrict v))
--                case r of
--                  Left err -> do
--                    close (unwords ["websockets output failed",show err])
--                    return False
--                  Right () -> return True

--              -- if there is data return it,
--              -- otherwise if connection is open retry otherwise return Nothing
--              inp = S.atomically $
--                ((Just <$> S.readTQueue q)
--                              <|> (do
--                                      connClosed <- S.readTVar closed
--                                      S.check connClosed
--                                      return Nothing
--                                  ))
--         app $ Connection inp out

        --putStrLn (show . L.unpack . flat . typedBytes $ (ByType::ByType Bool))
        -- putStrLn (show $ B.unpack . shake128 32 . B.pack $ [])
        -- app $ WSConnection {sendMsg = webSocketSend conn . L.toStrict
        --                    ,receiveMsg = undefined}

-- printB :: B.ByteString -> IO ()
-- printB = print

-- toBS :: ArrayBuffer -> B.ByteString
toBS = Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer
-- toBS1 = Buffer.toByteString 0 Nothing

-- CREDIT: adapted from https://github.com/reflex-frp/reflex-dom/blob/develop/src-ghcjs/Reflex/Dom/WebSocket/Foreign.hs
-- Websocketsend :: JSWebSocket -> ByteString -> IO ()
webSocketSend ws bs | B.length bs == 0 = return ()
                    | otherwise = do
                        let (b, off, len) = fromByteString bs
                        -- let ab = ArrayBuffer $ js_dataView off len (jsval $ getArrayBuffer b)
                        -- send ws (Just ab)
                        -- let ab = js_dataView off len (jsval $ getArrayBuffer b)
                        -- ?BUG:sending extra bytes
                        let ab = getArrayBuffer b
                        sendArrayBuffer ab ws
                        -- return ()

foreign import javascript safe "new DataView($3,$1,$2)" js_dataView :: Int -> Int -> JSVal -> JSVal

-- Temporary fix till this is sorted out: https://github.com/ghcjs/ghcjs-base/issues/60
getDataFixed :: MessageEvent -> Either String B.ByteString -- MessageEventData
getDataFixed me = case js_getData me of
                (# 1#, r #) -> Left "Unexpected String"
                (# 2#, r #) -> Left "Unexpected Blob"
                (# 3#, r #) -> Right $ toBS r -- ArrayBufferData ArrayBuffer -- (SomeArrayBuffer r)
{-# INLINE getDataFixed #-}

foreign import javascript unsafe
    "$r2 = $1.data;$r1 = typeof $r2 === 'string' ? 1 : ($r2 instanceof ArrayBuffer ? 3 : 2)"
    js_getData :: MessageEvent -> (# Int#, ArrayBuffer #)

-- By default, websockets would be in blob mode, so we need this.
foreign import javascript unsafe
    "$1.binaryType='arraybuffer'"
    js_asArrayBuffer :: WebSocket -> IO ()

#else
-- GHC Version
import qualified Network.WebSockets       as WS

-- |Run a WebSockets Application, keep connection alive.
-- Automatically close sockets on App exit
runWSClient :: Config -> WSApp r -> IO r
runWSClient cfg app =
     WS.runClientWith (cfgIP cfg) (cfgPort cfg) (cfgPath cfg) opts [("Sec-WebSocket-Protocol", "??")] $ \conn -> do
       -- WS.forkPingThread conn 20 -- Keep connection alive avoiding timeouts (FIX: the server should send pings as this is required by browsers)
       --WS.sendClose conn (1000::Int)
       -- app $ Connection
       --   (eitherToMaybe <$> tryE (WS.receiveData conn))
       --   (\bs -> isRight <$> tryE (WS.sendBinaryData conn bs))
       app $ Connection
          (WS.receiveData conn)
          (WS.sendBinaryData conn)
          (WS.sendClose conn ("So long, and thanks for all the fish!"::B.ByteString))

         where
     opts = WS.defaultConnectionOptions -- { WS.connectionOnPong = dbgS "gotPong"}

-- -- |Send a raw binary message on a WebSocket (untyped) connection
-- sendMsg :: WS.Connection -> L.ByteString -> IO ()
-- sendMsg = WS.sendBinaryData

-- -- |Receive a raw binary message from a WebSocket (untyped) connection
-- receiveMsg :: WS.Connection -> IO L.ByteString
-- receiveMsg conn = WS.receiveData conn

#endif

-- |Setup a connection by sending a value specifying the routing protocol to be used
-- protocol :: (Model (router a), Flat (router a)) => Connection a -> router a -> IO Bool
-- protocol conn =  send conn . typedBytes
protocol conn router = do
  dbg ["protocol",show router]
  output conn . flat . typedBytes $ router