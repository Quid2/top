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
import           Data.Int
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Pattern
import           Data.Typed
import           Data.Word
import           Language.Haskell.TH
import           Network.Top
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (sendCloseCode)
import           Repo.Memory
import qualified Repo.Types                    as R
import           System.Exit                   (exitFailure)
import           System.IO
import           System.Log.Logger
import           System.Time.Extra             (duration, showDuration)

main = do
  let dbgLevel = INFO
  --let dbgLevel = DEBUG
  -- updateGlobalLogger rootLoggerName $ setLevel dbgLevel
  logLevelOut dbgLevel stdout

  -- testRepo
  -- testTop
  testPatternConversion


t = do
  --recordType def (Proxy::Proxy Msg)
  -- recordType def (Proxy::Proxy R.RepoProtocol)
  -- recordType def (Proxy::Proxy (ChannelSelectionResult (WebSocketAddress IP4Address))) -- Repo)
  -- knownTypes def

  runClient def byAny $ \conn -> forever $ do
    print "WAIT"
    r <- input conn
    print r

m = runClient def (byPattern $(patternE [p|Msg "sj" _ Join|])) $ \conn -> do
  msg::Msg <- input conn
  print msg
  --exitFailure

pp = do
  --print $ (pat [p|False|] :: Bool)
  let p0 = $(patternE [p|0|])
  let p255 = $(patternE [p|255|])
  mapM_ print [
    w (byPattern $(patternE [p|False|]) :: ByPattern Bool)
    --,w (byPattern $(patternE [p|III {w8=11,w16=22}|]) :: ByPattern III)
    ,w (byPattern $(patternE [p|_|]) :: ByPattern Bool)
    ,w (byPattern $(patternE [p|Msg "sj" _ _|]) :: ByPattern Msg)
    ,w (byPattern $(patternE [p|Msg _ (Subject ("Hask":_:[])) (TextMsg "hello")|]) :: ByPattern Msg)
    ,w (byPattern $(patternE [p|(5,3.3,5.5)|]) :: ByPattern (Word16,Float,Double))
    ,w (byPattern $(patternE [p|(False,True,False,True)|]) :: ByPattern (Bool,Bool,Bool,Bool))
    ,w (byPattern p0 :: ByPattern Word8)
    ,w (byPattern p0 :: ByPattern Word16)
    ,w (byPattern p0 :: ByPattern Word32)
    ,w (byPattern p0 :: ByPattern Word64)
    ,w (byPattern p0 :: ByPattern Word)
    ,w (byPattern p0 :: ByPattern Int8)
    ,w (byPattern p0 :: ByPattern Int16)
    ,w (byPattern p0 :: ByPattern Int32)
    ,w (byPattern p0 :: ByPattern Int64)
    ,w (byPattern p0 :: ByPattern Int)
    ,w (byPattern p0 :: ByPattern Integer)
    ,w (byPattern p255 :: ByPattern Word8)
    ,w (byPattern p255 :: ByPattern Word16)
    ,w (byPattern p255 :: ByPattern Word32)
    ,w (byPattern p255 :: ByPattern Word64)
    ,w (byPattern p255 :: ByPattern Word)
    ,w (byPattern p255 :: ByPattern Int8)
    ,w (byPattern p255 :: ByPattern Int16)
    ,w (byPattern p255 :: ByPattern Int32)
    ,w (byPattern p255 :: ByPattern Int64)
    ,w (byPattern p255 :: ByPattern Int)
    ,w (byPattern p255 :: ByPattern Integer)
    ,w (byPattern p255 :: ByPattern Integer)
    ]

  --  where
  -- print $ (ByPattern $(pat [p|False|]) :: ByPattern Bool)
  --   pat pp = let ByPattern p = byPattern pp in p
  where
    --w :: ByPattern a -> Pattern WildCard
    w (ByPattern p) = p

testRepo = do
  -- Local type repo
  -- repo <- dbRepo "/tmp"
  repo <- memRepo
  let tm = absTypeModel (Proxy::Proxy AbsADT)
  r <- ((tm ==) <$>) <$> solveType repo def (typeName tm)
  R.close repo
  return r

p = testPatternConversion

testPatternConversion = do
  -- (pat [p|False|] :: IO Bool) >>= print
  print ""
  -- pats <- mapM patternQ [
  --   [p|_|]
  --   ,[p|33|]
  --   --,[p|33::Int#|]
  --    --,[p|-33|] -- unsupported by TH
  --   --,[p|3.3|]
  --   ,[p|III {w8=11}|]
  --   ,[p|Msg "sj" _ Join|]
  --   ,[p|Msg _ (Subject ("Haskell":_:[])) _|]
  --   ]    
  -- print pats
--    where pat hp = (\pp -> let ByPattern p = byPattern pp in p) <$> (patternQ hp)


-- TODO: test by sending incorrect router value
testTop = do

    let msgs = [Msg "rob" (Subject ["Joke"]) Join
               ,Msg "sj" (Subject ["Haskell"]) Join
               ,Msg "sj" (Subject ["Haskell"]) (TextMsg "All hail the boss!")
               ,Msg "titto" (Subject ["Haskell","General"]) (TextMsg "Nice language!")
               ]

    [pwild,p1,p2,p3,p4,p5,wp,fp,n1] <- mapM patternQ [
      [p|_|]
      ,[p|Msg "sj" _ _|]
      ,[p|Msg "sj" _ Join|]
      ,[p|Msg _ _ Join|]
      ,[p|Msg _ (Subject ("Haskell":[])) _|]
      ,[p|Msg _ (Subject ("Haskell":_:[])) _|]
      ,[p|33|]
      --,[p|-33|] -- unsupported by TH
      ,[p|3.3|]
      ,[p|III {w8=11}|]
      ]


    let mixTest0 = byMixedTest False 25 [
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

    -- NOTE: these tets (in particular byAny) will work only on an isolated router
    let mixAllTest = connTests [
          mix byAny [typedBLOB 'a',typedBLOB 'b'] 9
         ,mix byAny [] 11
         ,mix ByType msgs 0
         ,mix ByType [True,False,True] 2
         ,mix ByType [False,True] 3
         ,mix (byPattern p1) ([]::[Msg]) 2
         ,mix (byPattern pwild) ([]::[Bool]) 5
         ,mix (byPattern pwild) ([]::[Msg]) 4
         ,mix ByType ([]::[Char]) 2
         ,mix (byPattern pwild) ([]::[Char]) 2
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
      byTypeTest [TextMsg "ciao",Join,TextMsg "ok"] 7
      ,byTypeTest [False,True,False,True] 11
      ,mixTest
      --,mixTest0
      ,mixAllTest
      ,patTest
      ,patTest2
      ]

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

    -- Test low level protocol
    -- WebSockets should support up to 2**64 bytes long messages.
    -- wsTest :: ClientApp Bool
    -- NOTE: GHC only
    -- wsTest = (:[]) <$> wsTest_
    -- wsTest_ = run (Echo False::Echo [Int8]) $ \conn@(Connection connWS) -> do
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

byMixedTest precise multiplier hs msgs = do
  clts <- mapM
            (\(mp, num) -> let n = num * multiplier
                           in case mp of
                             Nothing -> return (run $ ByType, perClient n)
                             Just p  -> (\pat -> (run $ byPattern pat, perClient n)) <$> patternQ p)
            hs
  testClients clts

  where
    perClient numAnswers n conn = do
      when (n == 0) $ mapM_ (output conn) (concat $ replicate multiplier msgs)
      checkNumInputs precise numAnswers conn


byPatternTest ::  (Model a,Flat a,Show a,Foldable t,Show (t a)) => [(PatQ, Int)] -> t a -> IO [Async Bool]
byPatternTest hpats msgs = do
   pats <- mapM (\(p,i) -> (,i) <$> patternQ p) hpats
   testClients $ map act pats
  where
    act (pat,numAnswers) = (run $ byPattern pat,perClient)
      where
        perClient n conn = do
          when (n==0) $ mapM_ (output conn) msgs
          chkNumInputs numAnswers conn

-- Each actor sends the same messages and should receive all those sent minus those sent by itself.
byTypeTest :: (Model a,Flat a,Show a) => [a] -> Int -> IO [Async Bool]
byTypeTest msgs numDevices = do
  let numInMsgs = (numDevices-1)*length msgs
  testClients . map (run ByType,) . map (act numInMsgs) $ [1..numDevices]
  where act numInMsgs _ _ conn = do
          mapM_ (output conn) msgs
          chkNumInputs numInMsgs conn

chkNumInputs = checkNumInputs True

checkNumInputs precise numAnswers conn = do
  ans <- replicateM numAnswers (inputTimeout 10 conn)
  let allIn = all isJust ans
  -- WHY OH WHY? if inputWithTimeout returns a timeout, a ConnectionClosed exception is thrown after this returns
  -- after <- inputWithTimeout 3 conn
  -- So we do this way instead, we let the input continue undisturbed and we do not get an exception:
  after <- if precise then inputTimeout 5 conn else return Nothing
  let numFailed = length $ filter isNothing ans
  --when (numFailed >0) $ print (unints ["Failed",show numFailed,"out of",show numAnswers] )
  when (numFailed >0) $ print (unwords ["Failed",show numFailed,"out of",show numAnswers,show ans] )
  when (isJust after) $ print (unwords ["Failed after",show after] )
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
          waitAllStarted 
          --dbgS "All Started"
          -- Are these actually all started?
          -- Apparently not, so wait a bit longer
          threadDelay (secs 2)
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

data III = III {w8::Int8,w16::Int16,w::Int,i8::Int8,i::Int,f::Float,d::Double,ii::Integer}

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

