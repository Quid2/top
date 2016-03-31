{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric #-}
import           Control.Applicative
import           Control.Exception             (SomeException, catch, handle)
import           Data.Typed
import qualified Data.ByteString.Lazy          as BL
import           Data.Word                     (Word16)
import           Network.Router.API
import           System.Log.Logger
import           Control.Concurrent            (threadDelay)
import  qualified Network.WebSockets as WS
-- import           Network.WebSockets hiding (runClient,send,receive)
import           Network.WebSockets.Connection(sendCloseCode)
import Control.Concurrent.Async
import  Control.Concurrent.STM
import Data.Word

t = main

-- TODO: test by sending incorrect router value
main = do
    updateGlobalLogger rootLoggerName $ setLevel DEBUG -- INFO -- DEBUG

    tasks <- concat <$> sequence [--wsTest
                                 -- byTypeSimpleTest
                                 -- ,byTypeTest [True,False,True] 2
                                 byTypeTest [TextMsg "ciao",Join,TextMsg "ok"] 25
                                 ]
    mapM_ wait tasks

    -- -- let numDeviceMsgs = 3
    -- -- m <- run $ master numDevices numDeviceMsgs
    -- -- devices <- mapM (run . device numDevices numDeviceMsgs) [1..numDevices]
    -- -- let t2 = m:devices

  where

    msg1 = [1..40]

    msg n = BL.pack $ [fromIntegral n] --  : [1..40]

    msgL l = take l . concat . repeat $ [0..255]

    largeMsg = msgL 1000000

    -- master :: Int -> ClientApp Bool
    -- master numDevices numDeviceMsgs conn = do
    --   protocol conn $ NamedHub "huba"
    --   recMsgs conn $ numDevices*numDeviceMsgs
    --   return True

    -- device numDevices numDeviceMsgs id conn = do
    --    protocol conn $ NamedHub "huba"
    --    -- sendBinaryData conn (encode $ Named ("device"++show n) $ GeoPos (n*3.3) (n*5.5))
    --    threadDelay $ secs 1
    --    mapM_ (sendBinaryData conn . msg) [1 .. numDeviceMsgs]

    --    recMsgs conn $ (numDevices-1)*numDeviceMsgs
    --    return True

    -- Test low level protocol
    -- WebSockets should support up to 2**64 bytes long messages.
    -- wsTest :: ClientApp Bool
    wsTest = (:[]) <$> wsTest_
    wsTest_ = run (Echo False::Echo [Word8]) $ \conn@(Connection connWS) -> do
      let sendRec msg = do
            send conn msg
            msgRet <- receive conn
            threadDelay 0 -- 10000
            return $ msg == msgRet

      dbgS "wsTest"

      dbgS "wsTest1"
      echoOK <- sendRec $ msgL $ 100 --10 * 1000 * 1000000
      dbgS "wsTest2"

      -- About 10K round trips per sec, locally.
      -- chronoIO (mapM (const $ sendRec msg1) [1..10]) >>= chronoPrint "Send Rec Time"

      mapM (const $ sendRec msg1) [1..10]
      dbgS "wsTest3"
      let pingMsg = "What's a fish without an eye?" :: BL.ByteString
      WS.sendPing connWS pingMsg
      WS.ControlMessage (WS.Pong pongMsg) <- WS.receive connWS

      let closeCode = 11
      let closeMsg = ("Bye Byte" :: BL.ByteString)
      sendCloseCode connWS closeCode closeMsg
      ce <- expectCloseException connWS

      return $ echoOK && pingMsg==pongMsg && ce == Right (closeCode,closeMsg)

run router app = async $ do
   let cfg = def
   -- let cfg = def{ip="127.0.0.1"}--,path="/lll"}
   r <- runClient cfg router app
   dbg ["RUN RESULT",show router,show r]

byTypeSimpleTest = (:[]) <$> (run (ByType::ByType Char) (\conn -> mapM_ (send conn) ['a'..'c'] >> threadDelay (seconds 10)))

byTypeTest :: (Model a,Typed a,Flat a,Show a) => [a] -> Int -> IO [Async ()]
byTypeTest vs numDevices = do
  count <- newTVarIO 0
  mapM (\n -> run (ByType) $ \conn -> byTypeClient numDevices vs n count conn) [1..numDevices]

byTypeClient :: forall a. (Typed a,Flat a,Show a) => Int -> [a] -> Int -> TVar Int -> Connection a -> IO Bool
byTypeClient numDevices vs id count conn = do
   -- make sure all clients are connected or we will miss some messages.
  atomically $ modifyTVar' count (+1)
  waitAllStarted -- threadDelay $ secs 1 
  mapM_ (send conn) vs
  vs' :: [a] <- mapM (\_-> receive conn) [1 .. (numDevices-1)*length vs]
  return True
    where
      waitAllStarted = do
        c <- atomically $ readTVar count
        if c < numDevices
          then threadDelay (secs 1) >> waitAllStarted
          else return ()

recMsgs :: WS.Connection -> Int -> IO [BL.ByteString]
recMsgs conn n = mapM (\n -> WS.receiveData conn >>= \msg -> dbg ["RCV",show n,show msg] >> return msg) [1 .. n]

expectCloseException :: WS.Connection -> IO (Either String (Word16, BL.ByteString))
expectCloseException conn = act `catch` handler
    where
        act = do
          msg <- WS.receiveDataMessage conn
          return . Left . show . ("Unexpected data message " ++) . show $ msg
        handler (WS.CloseRequest i msg) = return $ Right (i,msg)
        handler e = return $ Left (show e)

secs = (* 1000000)


-- Data model for a simple chat system
data Msg = Msg {fromUser::User
               ,subject::Subject
               ,content::Content}
         deriving (Eq, Ord, Read, Show, Generic)

type User = String

-- Hierarchical subject
-- Example: Subject ["Haskell","Meeting","Firenze"]
data Subject = Subject [String] deriving (Eq, Ord, Read, Show, Generic)

-- Different kinds of contents
data Content =
              -- Basic text message
              TextMsg String
              | Join
 deriving (Eq, Ord, Read, Show, Generic) 

instance Flat Msg
instance Flat Subject
instance Flat Content

instance Model Msg
instance Model Subject
instance Model Content


