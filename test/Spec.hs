{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE GADTs                     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE StandaloneDeriving        #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
import           Control.Applicative
import           Control.Concurrent            (threadDelay)
import           Control.Concurrent.Async
import           Control.Concurrent.STM
import           Control.Exception             (SomeException, catch,
                                                fromException, handle)
import           Control.Monad
import qualified Data.ByteString.Lazy          as BL
import           Data.Either
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Pattern.TH
import           Data.Pattern.Types
import           Data.Typed
import           Data.Word                     (Word16)
import           Data.Word
import           Language.Haskell.TH
import           Network.Top
import           Network.Top.Repo
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (sendCloseCode)
-- import           Repo.Disk
import Repo.Memory
import qualified Repo.Types                    as R
import           System.Exit                   (exitFailure)
import           System.IO
import           System.Log.Logger
import           System.Time.Extra             (duration, showDuration)
-- import           System.Posix.Temp

main = do
  let dbgLevel = INFO
  --let dbgLevel = DEBUG
  -- updateGlobalLogger rootLoggerName $ setLevel dbgLevel
  logLevelOut dbgLevel stdout

  -- testRepo
  mainTest

t = runClient def $(byPattern [p|Msg "sj" _ Join|]) $ \conn -> do
  msg::Msg <- input conn
  print msg

testRepo = do
  -- Local type repo
  -- repo <- dbRepo "/tmp"
  repo <- memRepo
  let tm = absTypeModel (Proxy::Proxy AbsADT)
  r <- ((tm ==) <$>) <$> solveType repo def (typeName tm)
  R.close repo
  return r

showInfo = do
  --recordType def (Proxy::Proxy Msg)
  -- recordType def (Proxy::Proxy R.RepoProtocol)
  -- recordType def (Proxy::Proxy (ChannelSelectionResult (WebSocketAddress IP4Address))) -- Repo)
  knownTypes def

  -- exitFailure

-- TODO: test by sending incorrect router value
mainTest = do

    let msgs = [Msg "rob" (Subject ["Joke"]) Join
               ,Msg "sj" (Subject ["Haskell"]) Join
               ,Msg "sj" (Subject ["Haskell"]) (TextMsg "All hail the boss!")
               ,Msg "titto" (Subject ["Haskell","General"]) (TextMsg "Nice language!")
               ]

    [pwild,p1,p2,p3,p4,p5] <- mapM patternQ [
      [p|_|]
      ,[p|Msg "sj" _ _|]
      ,[p|Msg "sj" _ Join|]
      ,[p|Msg _ _ Join|]
      ,[p|Msg _ (Subject ("Haskell":[])) _|]
      ,[p|Msg _ (Subject ("Haskell":_:[])) _|]
      ]

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

    let mixAllTest = connTests [
          mix byAny [typedBLOB 'a',typedBLOB 'b'] 9
         ,mix byAny [] 11
         ,mix ByType msgs 0
         ,mix ByType [True,False,True] 2
         ,mix ByType [False,True] 3
         ,mix (ByPattern p1) ([]::[Msg]) 2
         ,mix (ByPattern pwild) ([]::[Bool]) 5
         ,mix (ByPattern pwild) ([]::[Msg]) 4
         ,mix ByType ([]::[Char]) 2
         ,mix (ByPattern pwild) ([]::[Char]) 2
         ]

    let patTest = byPatternTest [([p|_|],0) -- this sends so it receives nothing
                                ,([p|_|],4)
                                ,([p|Msg "sj" _ _|],2)
                                ,([p|Msg "sj" _ Join|],1)
                                ,([p|Msg _ _ Join|],2)
                                ,([p|Msg _ (Subject ("Haskell":[])) _|],2)
                                ,([p|Msg _ (Subject ("Haskell":_:[])) _|],1)
                                ] msgs

    let patTest2 = byPatternTest [([p|_|],0) -- this sends so it receives nothing
                                 ,([p|_|],3)
                                 ,([p|True:_|],2)
                                 ]
                   [[True,False,True],[False,False,True],[True,True]]

    mapM_ perform [--wsTest
      byTypeTest [TextMsg "ciao",Join,TextMsg "ok"] 3
      ,byTypeTest [False,True,False,True] 5
      ,mixTest
      ,mixAllTest
      ,patTest
      ,patTest2
      ]

    -- -- let numDeviceMsgs bS= 3
    -- -- m <- run $ master numDevices numDeviceMsgs
    -- -- devices <- mapM (run . device numDevices numDeviceMsgs) [1..numDevices]
    -- -- let t2 = m:devices

  where

    perform mtasks = do
      tasks <- mtasks
      (tr,r) <- duration ((\(ls,rs) -> catMaybes $ map (Just . show) ls ++ map (\r -> if r then Nothing else Just "Failed Test") rs) . partitionEithers <$> mapM waitCatch tasks)

      print $ if null r then "No Test Errors" else "Test Errors: " ++ intercalate " -- " r
      print $ showDuration tr

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

mix r msgs numAnswers = ConnTst (run r) act
  where
    act _ conn = do
      mapM_ (output conn) msgs
      checkNumInputs True numAnswers conn

-- byMixedAllTest cs = do
--    testClients $ map (\ -> (conn,perClient (out conn msgs) numAnswers)) cs
--   where
--     act msgs multiplier n conn =
--       mapM_ (output conn) (concat $ replicate multiplier msgs)
--     perClient (ch,msgs,numAnswers)act numAnswers conn = do
--       act
--       checkNumInputs precise (numAnswers*multiplier) conn
byMixedTest precise multiplier hs msgs = do
  clts <- mapM
            (\(mp, num) -> let n = num * multiplier
                           in case mp of
                             Nothing -> return (run $ ByType, perClient n)
                             Just p  -> (\pat -> (run $ ByPattern pat, perClient n)) <$> patternQ p)
            hs
  testClients clts

  where
    perClient numAnswers n conn = do
      when (n == 0) $ mapM_ (output conn) (concat $ replicate multiplier msgs)
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

-- Each actor sends the same messages and should receive all those sent minus those sent by itself.
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
  after <- if precise then inputTimeout 5 conn else return Nothing
  return $ allIn && isNothing after

inputTimeout :: Show a => Int -> Connection a -> IO (Maybe a)
inputTimeout secs conn = do
    t1 <- async $ input conn
    t2 <- async $ threadDelay (seconds secs)
    -- cannot cancel or we get a ConnectionClosed error so we have to leave threads running
    --after <- either (either (const Nothing) Just) (const Nothing) <$> waitEitherCatchCancel t1 t2
    after <- either (either (const Nothing) Just) (const Nothing) <$> waitEitherCatch t1 t2
    dbg ["inputTimeout",show after]
    return after

-- |A connection test
data ConnTst = forall a.
                           ConnTst
                             { tstStart :: App a Bool -> IO (Async Bool)
                             , tstBody :: Int -> Connection a -> IO Bool
                             }

testClients = connTests . map (uncurry ConnTst)

-- |Start all tests, when all have completed the start up/connection phase run them in parallel
connTests :: [ConnTst] -> IO [Async Bool]
connTests cls = do
  numStarted <- newTVarIO 0
  zipWithM (client numStarted) [0..] cls
    where
      client count n (ConnTst start act) = start $ \conn -> do
          -- make sure all clients are connected or we will miss some messages.

          atomically $ modifyTVar' count (+1)
          waitAllStarted -- Are these actually all started? Apparently not
          --dbgS "All Started"
          r <- act n conn
          dbg ["Result",show r]
          -- threadDelay (secs 10)
          return r
            where
            waitAllStarted = do
              c <- atomically $ readTVar count
              if c < length cls
                then threadDelay 100 >> waitAllStarted
                else return ()

-- testClients
--   :: (Enum t, Num t, Show b1) =>
--      [((t1 -> IO b1) -> IO b, t -> t1 -> IO b1)] -> IO [b]
-- testClients cls = do
--   count <- newTVarIO 0
--   let numDevices = length cls
--   mapM (\(n,c) -> client count numDevices n c) . zip [0..] $ cls
--     where
--       client count numDevices n (r,act) = r $ \conn -> do
--           -- make sure all clients are connected or we will miss some messages.

--           atomically $ modifyTVar' count (+1)
--           waitAllStarted -- Are these actually all started? Apparently not
--           --dbgS "All Started"
--           r <- act n conn
--           dbg ["Result",show r]
--           -- threadDelay (secs 30)
--           return r
--             where
--             waitAllStarted = do
--               c <- atomically $ readTVar count
--               if c < numDevices
--                 then threadDelay 100 >> waitAllStarted
--                 else return ()

-- waitCatchExceptClose task = exp <$> waitCatch task
--   where
--     exp (Left e) | fromException e == Just WS.ConnectionClosed = Right True
--     exp e = e

run
  :: (Show a, Show a1, Show (router a1), Model (router a1), Flat a1,
      Flat (router a1)) =>
     router a1 -> App a1 a -> IO (Async a)
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

deriving instance Eq (WS.ConnectionException)

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

