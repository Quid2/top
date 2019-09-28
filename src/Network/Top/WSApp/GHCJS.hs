{-# LANGUAGE CPP #-}

{-# LANGUAGE MagicHash     #-}
{-# LANGUAGE UnboxedTuples #-}

-- {-# LANGUAGE ForeignFunctionInterface  #-}
-- {-# LANGUAGE GHCForeignImportPrim      #-}
-- {-# LANGUAGE JavaScriptFFI             #-}
{-# LANGUAGE UnliftedFFITypes          #-}

-- {-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE OverloadedStrings         #-}
-- {-# LANGUAGE ScopedTypeVariables       #-}
module Network.Top.WSApp.GHCJS
  ( 
#ifdef ghcjs_HOST_OS
  runWSApp
#endif
  ) where

#ifdef ghcjs_HOST_OS

import qualified Data.ByteString                   as B
import           Network.Top.Types

import           Control.Applicative               (Alternative (empty, (<|>)))
import qualified Control.Concurrent.STM            as S
import           Control.Exception
import qualified Data.ByteString.Lazy              as L
import qualified Data.JSString                     as S
import           Data.Maybe
import qualified Data.Text                         as T
import           GHC.Exts
import           GHCJS.Buffer
import qualified GHCJS.Buffer                      as Buffer
import           GHCJS.Foreign
import           GHCJS.Foreign.Internal
import           GHCJS.Marshal
import           GHCJS.Marshal.Pure
import           GHCJS.Types
import           JavaScript.TypedArray.ArrayBuffer
import qualified JavaScript.TypedArray.ArrayBuffer as A
import           JavaScript.Web.Blob.Internal      (Blob, SomeBlob (..))
import           JavaScript.Web.MessageEvent
import           JavaScript.Web.WebSocket          hiding (close)
import qualified JavaScript.Web.WebSocket          as JS
import           Network.Top.Util

-- import JavaScript.TypedArray -- ArrayBuffer, SomeArrayBuffer(..))
-- import JavaScript.TypedArray.Internal -- ArrayBuffer, SomeArrayBuffer(..))
runWSApp cfg = bracket (newConnection cfg) close

-- |A WS connection
data Conn a =
  Conn
    { connConfig   :: Config
    , connStatus   :: S.TVar (ConnStatus a)
    , connMessages :: S.TQueue a
    }

-- |Connection Status
data ConnStatus a
  = ConnOpening
  | ConnOpen
      { inp :: IO a
      , out :: a -> IO ()
      , cls :: IO ()
      }
  | ConnClosed -- is this needed?

newConnection :: Config -> IO WSConnection
newConnection cfg = do
  conn <- Conn cfg <$> S.newTVarIO ConnClosed <*> S.newTQueueIO
  -- BUG: reopen connection without sending the protocol
  -- return $ Connection
  --    (open conn >>= \(i,o,c) -> i)
  --    (\v -> open conn >>= \(i,o,c) -> o v)
  --    (tillClose $ connStatus conn)
  (i, o, c) <- open conn
  return $ Connection i o (tillClose $ connStatus conn)
  where
    tillClose st = do
      dbgS "[Close"
      toClose <-
        S.atomically $ do
          s <- S.readTVar st
          case s of
            ConnOpening      -> S.retry
            ConnOpen _ _ cls -> return $ Just cls
            ConnClosed       -> return Nothing
      fromMaybe (return ()) toClose
      dbgS "Close]"

-- changeStatus conn = S.atomically $ S.writeTVar (connStatus conn) ConnClosed
-- |Block till open
open c =
  fromMaybe <$> reopen c <*>
  (S.atomically $ do
     s <- S.readTVar (connStatus c)
     case s of
       ConnOpening -> S.retry
       ConnOpen i o c -> return $ Just (i, o, c)
       ConnClosed -> do
         S.writeTVar (connStatus c) ConnOpening
         return Nothing)

reopen c = do
  dbgS "[reopen"
  let cfg = connConfig c
  econn <-
    tryE
      (JS.connect $
       JS.WebSocketRequest
         { url =
             S.pack $
             concat ["ws://", cfgIP cfg, ":", show (cfgPort cfg), cfgPath cfg]
         , protocols = [S.pack $ T.unpack $ chatsProtocolT]
         , onClose = Just $ \_ -> closeConn c
         , onMessage =
             Just $ \event -> do
               let msg = getMessage event
              
               case msg of
                 Left e -> error e
                 Right ar -> do
                    let bs = toBS ar               
                    -- dbgS $ "received binary message of length " ++ show (let Right ar = msg in A.byteLength ar) ++ " bytestring = " ++ show bs
                    S.atomically . S.writeTQueue (connMessages c) $ bs
         })
  case econn of
    Left e -> do
      dbgS "Error while opening connection"
      threadDelay (seconds 5)
      reopen c
    Right ws
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
     -> do
      let inp = do
            r <- S.atomically $ S.readTQueue (connMessages c)
               -- dbg ["received",show $ L.unpack r]
            return r
          out v
               --dbg ["send",show $ unpack v]
           = do
            webSocketSend ws v
             -- out v = do
              --   r <- tryE (webSocketSend conn (L.toStrict v))
             --   case r of
             --     Left err -> do
             --       cls (unwords ["websockets output failed",show err])
             --       (_,o,_) <- reopen c
             --       o v
             --     Right () -> return ()
          cls reason
               -- closeConn conn reason
           = do
            JS.close Nothing Nothing ws -- (S.pack "FIX THIS") ws
            dbg [reason, "closed]"]
      let cls' = (cls "user request")
      js_asArrayBuffer ws
      S.atomically $ S.writeTVar (connStatus c) (ConnOpen inp out cls')
      dbgS "reopen]"
      return (inp, out, cls')
  where
    closeConn conn = S.atomically $ S.writeTVar (connStatus conn) ConnClosed
 --                 ArrayBufferData ab -> S.atomically . S.writeTQueue q . L.fromStrict . toBS $ ab
  -- ArrayBufferData ab -> Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer $ ab
        --putStrLn (show . L.unpack . flat . typedBytes $ (ByType::ByType Bool))
        -- putStrLn (show $ B.unpack . shake128 32 . B.pack $ [])
        -- app $ WSConnection {sendMsg = webSocketSend conn . L.toStrict
        --                    ,receiveMsg = undefined}

-- closeConn conn reason = do
--    JS.close conn
--    dbg ["Closing connection",reason]
-- runWSApp :: Config -> WSApp r -> IO r
-- runWSApp cfg app = do
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
-- printB :: B.ByteString -> IO ()
-- printB = print

-- NOTE: even if the ArrayBuffer is immutable,
-- it will give an error if is not copied.
toBS :: ArrayBuffer -> B.ByteString
toBS = B.copy . Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer
--toBS ar = Buffer.toByteString 0 Nothing . Buffer.createFromArrayBuffer <$> Buffer.freeze ar

-- toBS1 = Buffer.toByteString 0 Nothing
webSocketSend :: WebSocket -> B.ByteString -> IO ()
webSocketSend ws bs
  | B.length bs == 0 = return ()
  | otherwise = do
    let (b, off, len) = fromByteString bs
                        -- dbgS $ unwords ["BUFFER OFF",show off,"LEN",show len]
    js_sendArrayBuffer ws (getArrayBuffer b) off len

-- foreign import javascript safe "new DataView($3,$1,$2)" js_dataView
--   :: Int -> Int -> JSVal -> JSVal

-- getMessageAsBS :: MessageEvent -> Either String B.ByteString
-- getMessageAsBS event  = toBS <$> getMessage event

getMessage :: MessageEvent -> Either String ArrayBuffer
getMessage msg = 
    case getData msg of
            StringData _ -> Left $ "Unexpected String"
            BlobData _ -> Left "Unexpected Blob"
            ArrayBufferData d -> Right d
            
        --fromMaybe (Left "not a binary message") <$> fromJSVal val  

-- NOTE: fix for older ghcjs-base
-- {-# INLINE getMessageAsBS #-}
-- getMessageAsBS me =
--   case js_getData me of
--     (# 1#, r #) -> Left "Unexpected String"
--     (# 2#, r #) -> Left "Unexpected Blob"
--     (# 3#, r #) -> Right $ toBS r

-- foreign import javascript unsafe "$r2 = $1.data;$r1 = typeof $r2 === 'string' ? 1 : ($r2 instanceof ArrayBuffer ? 3 : 2)" js_getData
--     :: MessageEvent -> (# Int#, ArrayBuffer #)
  

foreign import javascript unsafe "$1.send(new Uint8Array($2,$3,$4))" js_sendArrayBuffer
  :: WebSocket -> ArrayBuffer -> Int -> Int -> IO ()

-- By default, websockets would be in blob mode, so we need this.
foreign import javascript unsafe "$1.binaryType='arraybuffer'" js_asArrayBuffer
  :: WebSocket -> IO ()

#endif
