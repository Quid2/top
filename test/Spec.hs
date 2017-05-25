{-# LANGUAGE DeriveAnyClass            #-}
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
import           Data.Word
import           Language.Haskell.TH
import           Network.Top
import qualified Network.WebSockets            as WS
import           Network.WebSockets.Connection (sendCloseCode)
import           System.Exit                   (exitFailure)
import           System.IO
import           System.Log.Logger
import           System.Time.Extra             (duration, showDuration)
import           ZM

t = main

main = do
  let dbgLevel = INFO
  -- let dbgLevel = DEBUG
  -- updateGlobalLogger rootLoggerName $ setLevel dbgLevel
  logLevelOut dbgLevel stdout

  testTop
  --exitFailure

m = runApp def (byPattern $(patternE [p|Msg "sj" _ Join|])) $ \conn -> do
  msg::Msg <- input conn
  print msg
  --exitFailure

  --  where
  -- print $ (ByPattern $(pat [p|False|]) :: ByPattern Bool)
  --   pat pp = let ByPattern p = byPattern pp in p
  where
    --w :: ByPattern a -> Pattern WildCard
    w (ByPattern p) = p

-- TODO: test by sending incorrect router value
testTop = do

    -- We need to save the definitions of the types we use
    --saveTypes

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
      ,[p|Msg _ (Subject ["Haskell"]) _|]
      ,[p|Msg _ (Subject ["Haskell", _]) _|]
      ,[p|33|]
      --,[p|-33|] -- unsupported by TH
      ,[p|3.3|]
      ,[p|III {w8=11}|]
      ]

    -- NOTE: these tests (in particular byAny) will work only on an isolated router
    let mixTest0 = connTests "Mix"[
          tany [] 11

          ,tany [typedBLOB 'a',typedBLOB 'b'] 9
          ,ttyp ([]::String) 2
          ,mix (byPattern pwild) ([]::String) 2

          ,ttyp msgs 0
          ,mix (byPattern p1) ([]::[Msg]) 2
          ,mix (byPattern pwild) ([]::[Msg]) 4

          ,ttyp [True,False,True] 2
          ,ttyp [False,True] 3
          ,mix (byPattern pwild) ([]::[Bool]) 5
          ]

    let patTest = connTests "ByPattern Msg" [
           tpat $(patternE [p|_|])                                  msgs        0
          ,tpat $(patternE [p|_|])                                  ([]::[Msg]) 4
          ,tpat $(patternE [p|Msg "sj" _ _|])                       ([]::[Msg]) 2
          ,tpat $(patternE [p|Msg "sj" _ Join|])                    ([]::[Msg]) 1
          ,tpat $(patternE [p|Msg _ _ Join|])                       ([]::[Msg]) 2
          ,tpat $(patternE [p|Msg _ (Subject ("Haskell":[])) _|])   ([]::[Msg]) 2
          ,tpat $(patternE [p|Msg _ (Subject ("Haskell":_:[])) _|]) ([]::[Msg]) 1
          ]

    let mixTest1 = connTests "ByPattern [Bool]" [
          tpat $(patternE [p|_|])        [[False]] 3
          ,tpat $(patternE [p|_|])       [[True,False,True]] 3
          ,tpat ($(patternE[p|True:_|])) [[True,True]] 2
          ,ttyp                          [[True]] 3
          ]

    let byTypeTest = connTests "ByType [Bool]" [
          ttyp  [[False]] 4
          ,ttyp [[True,False,True]] 4
          ,ttyp [[True,True]] 4
          ,ttyp [[True],[False]] 3
          ]

    let unknownTest = connTests "Unknown" [
          tpat $(patternE [p|_|])        [Unknown] 0
          --,ttyp                          [Unknown] 0
          ]

    -- Test have to be performed sequentially as they are not independent (use common channels)
    -- PROB: THis will fail on first test failure
    mapM_ perform [
      mixTest0,patTest,mixTest1,byTypeTest

      --unknownTest
      ]


perform tests = do
      -- tasks :: [Async (Maybe Error)] <- concat <$> sequence tests
      tasks :: [Async (Maybe Error)] <- tests 
      (durationTime,errs) <- duration ((\(ls,rs) -> catMaybes $ map (Just . show) ls ++ rs) . partitionEithers <$> mapM waitCatch tasks)

      putStrLn $ unwords ["Total duration:",showDuration durationTime]
      if null errs
        then putStrLn $ "No Test Errors"
        else do
        mapM_ putStrLn errs
        exitFailure

    --tpat :: forall t. Model t => (Pat PRef) -> 

ttyp
  :: (Flat a, Model a, Show a, Foldable t) => t a -> Int -> ConnTst
ttyp = mix ByType

tpat
  :: (Flat a, Model a, Show a, Foldable t) =>
     Data.Pattern.Pat PRef -> t a -> Int -> ConnTst
tpat pat = mix (byPattern pat)

tany = mix byAny

--byTypeSimpleTest :: IO [Async ()]
-- byTypeSimpleTest = (:[]) <$> (run (ByType::ByType Char) (\conn -> mapM_ (output conn) ['a'..'c'] >> threadDelay (seconds 10)))

mix ::
  (Flat (router a), Flat a, Model (router a), Show (router a),
   Show a, Foldable t) =>
  router a -> t a -> Int -> ConnTst
mix protocol msgsToSend numExpectedAnswers = ConnTst (run protocol) act
  where
    act _ conn = do
      dbgS "sending msgs"
      mapM_ (output conn) msgsToSend
      checkInputs numExpectedAnswers conn

-- |Execute a set of either ByType or ByPattern tests
-- byMixedTest
--   :: (Show a, Model a, Flat a) =>
--      Bool    -- ^True for precise matching of number of returned replies
--      -> Int  -- ^How many times to send group of test messages
--      -> [(Maybe (Q Language.Haskell.TH.Pat), Int)] -- ^Test clients: for each, an optional pattern and the number of replies expected
--      -> [a]                     -- ^Messages to send (all sent by first test client)
--      -> IO [Async Bool]
-- byMixedTest precise multiplier tests msgs = do
--   clients <- mapM
--             (\(mp, num) -> let n = num * multiplier
--                            in case mp of
--                              Nothing -> return (run ByType, perClient n)
--                              Just p  -> (\pat -> (run $ byPattern pat, perClient n)) <$> patternQ p)
--             tests
--   testClients clients

--   where
--     perClient numAnswers clientID conn = do
--       when (clientID == 0) $ mapM_ (output conn) (concat $ replicate multiplier msgs)
--       checkNumInputs precise numAnswers conn


-- byPatternTest ::  (Model a,Flat a,Show a,Foldable t,Show (t a)) => [(PatQ, Int)] -> t a -> IO [Async Bool]
-- byPatternTest hpats msgs = do
--    pats <- mapM (\(p,i) -> (,i) <$> patternQ p) hpats
--    testClients $ map act pats
--   where
--     act (pat,numAnswers) = (run $ byPattern pat,perClient)
--       where
--         perClient n conn = do
--           when (n==0) $ mapM_ (output conn) msgs
--           chkNumInputs numAnswers conn

-- -- Each actor sends the same messages and should receive all those sent minus those sent by itself.
-- byTypeTest :: (Model a,Flat a,Show a) => [a] -> Int -> IO [Async Bool]
-- byTypeTest msgs numDevices = do
--   let numInMsgs = (numDevices-1)*length msgs
--   testClients . map (run ByType,) . map (act numInMsgs) $ [1..numDevices]
--   where act numInMsgs _ _ conn = do
--           mapM_ (output conn) msgs
--           chkNumInputs numInMsgs conn

-- chkNumInputs = checkNumInputs True

checkInputs numExpectedAnswers conn = do
  ans <- replicateM numExpectedAnswers (inputTimeout 10 conn)
  let numAnswers = length $ filter isJust ans
  --let allIn = all isJust ans
  -- WHY OH WHY? if inputWithTimeout returns a timeout, a ConnectionClosed exception is thrown after this returns
  -- after <- inputWithTimeout 3 conn
  -- So we do this way instead, we let the input continue undisturbed and we do not get an exception:
  nothingAfter <- isNothing <$> inputTimeout 5 conn
  --after <- if precise then inputTimeout 5 conn else return Nothing

  --when (numFailed >0) $ print (unints ["Failed",show numFailed,"out of",show numAnswers] )
  --when (numFailed >0) $ print (unwords ["Failed",show numFailed,"message reads out of",show numAnswers,show ans] )
  --when (isJust after) $ print (unwords ["Failed after",show after] )
  return $ if numAnswers == numExpectedAnswers && nothingAfter
           then Nothing
           else Just $ unwords ["Expected"
                               ,show numExpectedAnswers
                               ,"but got"
                               ,if numAnswers < numExpectedAnswers
                                then show numAnswers
                                else "more than " ++ show numExpectedAnswers]


checkNumInputs
  precise  -- If True check that the exact number of messages has been received, otherwise that at least the number of messages has been received
  numAnswers
  --expectedMessages
  conn = do
  ans <- replicateM numAnswers (inputTimeout 10 conn)
  let allIn = all isJust ans
  -- WHY OH WHY? if inputWithTimeout returns a timeout, a ConnectionClosed exception is thrown after this returns
  -- after <- inputWithTimeout 3 conn
  -- So we do this way instead, we let the input continue undisturbed and we do not get an exception:
  after <- if precise then inputTimeout 5 conn else return Nothing
  let numFailed = length $ filter isNothing ans
  --when (numFailed >0) $ print (unints ["Failed",show numFailed,"out of",show numAnswers] )
  when (numFailed >0) $ print (unwords ["Failed",show numFailed,"message reads out of",show numAnswers,show ans] )
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
data ConnTst =
  forall msgType. ConnTst
  { -- |Connection phase
    tstStart :: App msgType (Maybe Error) -> IO (Async (Maybe Error))
    -- |Test phase
  , tstBody :: Int                 -- ^Client identifier
            -> Connection msgType  -- ^Connection to test
            -> IO (Maybe Error)    -- ^Test Result
  }

type Error = String

--testClients = connTests . map (uncurry ConnTst)

-- |Start all tests, when all have completed the start up/connection phase run them in parallel
-- Return results
-- We need to make sure all test clients are connected or we will miss some messages.
-- Problem: if one client does not get a valid connection we will wait for ever.
connTests :: String -> [ConnTst] -> IO [Async (Maybe String)]
connTests testName cls = do
  numStarted <- newTVarIO 0
  zipWithM (\clientID tst -> (((\r -> unwords [testName,"Test Number",show $ clientID+1,r]) <$>) <$>) <$> client numStarted clientID tst) [0..] cls
    where
      client count clientID (ConnTst start act) = start $ \conn -> do
          dbgS $ "Started " ++ show clientID
          atomically $ modifyTVar' count (+1)
          waitAllStarted
          -- Are these actually all started?
          -- Apparently not, so wait a bit longer
          threadDelay (secs 2)
          dbgS "All Started"
          r <- act clientID conn
          dbg ["Result",show r]
          -- threadDelay (secs 10)
          return r
            where
            waitAllStarted = do
              c <- atomically $ readTVar count
              Control.Monad.when (c < length cls) $ threadDelay 100 >> waitAllStarted

-- waitCatchExceptClose task = exp <$> waitCatch task
--   where
--     exp (Left e) | fromException e == Just WS.ConnectionClosed = Right True
--     exp e = e

run router app = async $ do
   let cfg = def
   -- let cfg = def{ip="127.0.0.1"}--,path="/lll"}
   er <- try $ runApp cfg router app
   dbg ["RUN RESULT",show router,show er]
   return $ case er of
     Right mr -> mr
     Left (er::SomeException) -> Just (show er)

-- |Wait for and return the indicated number of messages
recMsgs :: WS.Connection -> Int -> IO [BL.ByteString]
recMsgs conn n = mapM (\n -> WS.receiveData conn >>= \msg -> dbg ["RCV",show n,show msg] >> return msg) [1 .. n]

printAllMessages =
  runApp def byAny $ \conn -> forever $ do
  print "WAIT"
  r <- input conn
  print r

-- Test data types
saveTypes = do
    recordType def (Proxy::Proxy III)
    recordType def (Proxy::Proxy Msg)
    recordType def (Proxy::Proxy [Bool])

data III = III {w8::Int8,w16::Int16,w::Int,i8::Int8,i::Int,f::Float,d::Double,ii::Integer}
  deriving (Eq, Ord, Read, Show, Generic,Flat, Model)

-- Data model for a simple chat system
data Msg = Msg {fromUser::User
               ,subject::Subject
               ,content::Content}
         deriving (Eq, Ord, Read, Show, Generic,Flat, Model)

type User = String

-- Hierarchical subject
-- Example: Subject ["Haskell","Meeting","Firenze"]
data Subject = Subject [String] deriving (Eq, Ord, Read, Show, Generic, Flat, Model)

-- Different kinds of contents
data Content =
              -- Basic text message
              TextMsg String
              | Join
 deriving (Eq, Ord, Read, Show, Generic, Flat, Model)

data Unknown = Unknown
         deriving (Eq, Ord, Read, Show, Generic,Flat, Model)


-- Utilities
secs = (* 1000000)

deriving instance Eq WS.ConnectionException
