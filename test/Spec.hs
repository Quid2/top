{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE DeriveGeneric ,StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    ,TupleSections       #-}
import Control.Monad
import           Control.Applicative
import           Control.Exception             (SomeException, catch, handle,fromException)
import           Data.Typed
import qualified Data.ByteString.Lazy          as BL
import           Data.Word                     (Word16)
import           Network.Top
import           System.Log.Logger
import           Control.Concurrent            (threadDelay)
import  qualified Network.WebSockets as WS
import           Network.WebSockets.Connection(sendCloseCode)
import Control.Concurrent.Async
import  Control.Concurrent.STM
import Data.Word
import Data.Maybe
import Data.Either
import Data.Pattern.Types
import Language.Haskell.TH
import System.IO
import Data.List
import Data.IORef
import Repo.DB
import Repo.Types
import Network.Top.Repo
import System.Time.Extra(duration,showDuration)


deriving instance Eq (WS.ConnectionException)

main = mainTest
--main = mainRepo

mainRepo = do
  --recordType def (Proxy::Proxy Msg)

  -- Local type repo
  db <- openDB "/tmp"
  let dbRepo = Repo {get = \ref -> do
                        mr <- getDB db ref
                        --print (unwords ["get",show ref,show mr])
                        print (unwords ["get",show ref,show $ isJust mr])
                        return mr
                    ,put = \adt -> do
                        print (unwords ["put",prettyShow adt])
                        putDB db (refS adt) adt
                    }

  er <- solveProxy dbRepo def (Proxy::Proxy Msg) -- AbsADT) --  [Bool])
  case er of
    Left err -> print "Failed" >> error (err)
    Right r -> putStrLn $ prettyShow r

-- TODO: test by sending incorrect router value
mainTest = do
    let dbgLevel = INFO -- DEBUG
    -- updateGlobalLogger rootLoggerName $ setLevel dbgLevel
    logLevelOut dbgLevel stdout

    --recordType def (Proxy::Proxy Msg)

    let msgs = [Msg "rob" (Subject ["Joke"]) Join
               ,Msg "sj" (Subject ["Haskell"]) Join
               ,Msg "sj" (Subject ["Haskell"]) (TextMsg "All hail the boss!")
               ,Msg "titto" (Subject ["Haskell","General"]) (TextMsg "Nice language!")
               ]

    let patTest = byPatternTest [([p|_|],0) -- this sends so it receives nothing
                                ,([p|_|],4)
                                ,([p|Msg "sj" _ _|],2)
                                ,([p|Msg "sj" _ Join|],1)
                                ,([p|Msg _ _ Join|],2)
                                ,([p|Msg _ (Subject ("Haskell":[])) _|],2)
                                ,([p|Msg _ (Subject ("Haskell":_:[])) _|],1)
                                ] msgs

    let mixTest0 = byMixedTest False 2500 [
          (Nothing,0) -- this sends so it receives nothing
          ,(Just [p|_|],4)
          ,(Nothing,4)
          ,(Just [p|Msg "sj" _ _|],2)
          ,(Just [p|Msg "sj" _ Join|],1)
          ,(Just [p|Msg _ _ Join|],2)] msgs

    let mixTest = byMixedTest True 5 [
          (Nothing,0) -- this sends so it receives nothing
          ,(Nothing,4)
          ,(Nothing,4)
          ,(Nothing,4)
          ,(Nothing,4)
          ,(Nothing,4)
          ] msgs

    let patTest2 = byPatternTest [([p|_|],0) -- this sends so it receives nothing
                                 ,([p|_|],3)
                                 ,([p|True:_|],2)
                                 ]
                   [[True,False,True],[False,False,True],[True,True]]

    let typTest = byTypeTest [TextMsg "ciao",Join,TextMsg "ok"] 3
    let typTest2 = byTypeTest [False,True,False,True] 5
    tasks <- concat <$> sequence [--wsTest
                                 -- byTypeSimpleTest
                                 --byTypeTest [True,False,True] 2,

                                 -- patTest,patTest2
                                 --,typTest,typTest2
                                 mixTest
                                 ]
    -- r <- (\(ls,rs) -> catMaybes $ map (const $ Just "Interrupted Test") ls ++ map (\r -> if r then Nothing else Just "Failed Test") rs) . partitionEithers <$> mapM waitCatch tasks

    let runTask = waitCatch
    -- let runTask =

    (tr,r) <- duration ((\(ls,rs) -> catMaybes $ map (Just . show) ls ++ map (\r -> if r then Nothing else Just "Failed Test") rs) . partitionEithers <$> mapM runTask tasks)

    print $ if null r then "No Test Errors" else "Test Errors: " ++ intercalate " -- " r
    print $ showDuration tr

    -- -- let numDeviceMsgs bS= 3
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
    -- NOTE: GHC only
    -- wsTest = (:[]) <$> wsTest_
    -- wsTest_ = run (Echo False::Echo [Word8]) $ \conn@(Connection connWS) -> do
    --   let sendRec msg = do
    --         output conn msg
    --         msgRet <- input conn
    --         threadDelay 0 -- 10000
    --         return $ msg == msgRet

    --   dbgS "wsTest"

    --   dbgS "wsTest1"
    --   echoOK <- sendRec $ msgL $ 100 --10 * 1000 * 1000000
    --   dbgS "wsTest2"

    --   -- About 10K round trips per sec, locally.
    --   -- chronoIO (mapM (const $ sendRec msg1) [1..10]) >>= chronoPrint "Send Rec Time"

    --   mapM (const $ sendRec msg1) [1..10]
    --   dbgS "wsTest3"
    --   let pingMsg = "What's a fish without an eye?" :: BL.ByteString
    --   WS.sendPing connWS pingMsg
    --   WS.ControlMessage (WS.Pong pongMsg) <- WS.receive connWS

    --   let closeCode = 11
    --   let closeMsg = ("Bye Byte" :: BL.ByteString)
    --   sendCloseCode connWS closeCode closeMsg
    --   ce <- expectCloseException connWS

    --   return $ echoOK && pingMsg==pongMsg && ce == Right (closeCode,closeMsg)

--byTypeSimpleTest :: IO [Async ()]
-- byTypeSimpleTest = (:[]) <$> (run (ByType::ByType Char) (\conn -> mapM_ (output conn) ['a'..'c'] >> threadDelay (seconds 10)))

byMixedTest precise multiplier hs msgs = do
   clts <- mapM (\(mp,num) -> let n = num * multiplier
                              in case mp of
                                   Nothing -> return (run $ ByType,perClient n)
                                   Just p -> (\pat -> (run $ ByPattern pat,perClient n)) <$> patternQ p
                ) hs
   testClients clts
  where
    perClient numAnswers n conn = do
      when (n==0) $ mapM_ (output conn) (concat $ replicate multiplier msgs)
      checkNumInputs precise numAnswers conn

byPatternTest ::  (Model a,Typed a,Flat a,Show a,Foldable t,Show (t a)) => [(PatQ, Int)] -> t a -> IO [Async Bool]
byPatternTest hpats msgs = do
   pats <- mapM (\(p,i) -> (,i) <$> patternQ p) hpats
   testClients $ map act pats
  where
    act (pat,numAnswers) = (run $ ByPattern pat,perClient)
      where
        perClient n conn = do
          when (n==0) $ mapM_ (output conn) msgs
          chkNumInputs numAnswers conn

byTypeTest :: (Model a,Typed a,Flat a,Show a) => [a] -> Int -> IO [Async Bool]
byTypeTest msgs numDevices = do
  let numInMsgs = (numDevices-1)*length msgs
  testClients . map (run ByType,) . map (act numInMsgs) $ [1..numDevices]
  where act numInMsgs _ _ conn = do
          mapM_ (output conn) msgs
          chkNumInputs numInMsgs conn

chkNumInputs = checkNumInputs True

checkNumInputs precise numAnswers conn = do
  allIn <- all isJust <$> replicateM numAnswers (inputTimeout 10 conn)
  -- WHY OH WHY? if inputWithTimeout returns a timeout, a ConnectionClosed exception is thrown after this returns
  -- after <- inputWithTimeout 3 conn
  -- So we do this way instead, we let the input continue undisturbed and we do not get an exception:
  after <- if precise then inputTimeout 3 conn else return Nothing
  return $ allIn && isNothing after

-- BUG: this leaves hanging threads
inputTimeout secs conn = do
    t1 <- async $ input conn
    t2 <- async $ threadDelay (seconds secs)
    after <- either Just (const Nothing) <$> waitEither t1 t2
    dbg ["inputTimeout",show after]
    return after

testClients cls = do
  count <- newTVarIO 0
  let numDevices = length cls
  mapM (\(n,c) -> client count numDevices n c) . zip [0..] $ cls
    where
      client count numDevices n (r,act) = r $ \conn -> do
          -- make sure all clients are connected or we will miss some messages.

          atomically $ modifyTVar' count (+1)
          waitAllStarted -- Are these actually all started? Apparently not
          --dbgS "All Started"
          r <- act n conn
          dbg ["Result",show r]
          --threadDelay (secs 30)
          return r
            where
            waitAllStarted = do
              c <- atomically $ readTVar count
              if c < numDevices
                then threadDelay 100 >> waitAllStarted
                else return ()

-- waitCatchExceptClose task = exp <$> waitCatch task
--   where
--     exp (Left e) | fromException e == Just WS.ConnectionClosed = Right True
--     exp e = e

run router app = async $ do
-- run router app = do
   let cfg = def
   -- let cfg = def{ip="127.0.0.1"}--,path="/lll"}
   r <- runClient cfg router app
   dbg ["RUN RESULT",show router,show r]
   return r

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

