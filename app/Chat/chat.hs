{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}
{-
TODOs:
-- .. too many to list
-}
-- |A very basic chat client
module Main where

import           Chat.Model
import           Chat.Util
import           Control.Concurrent.Async
import           Control.Monad
import           Control.Monad.Trans.State
import           Data.Pattern
import           Network.Quid2             hiding (cat)
import           Pipes                     (cat, for)
import           Pipes.Prelude             (stdinLn)
import           System.Environment

-- Program parameters: <Input> <Output> <Debug>
-- To avoid problems with user input and message mixing up in the terminal,
-- run one instance in Input and another in Output mode.

data Mode = Input
          | Output
          | Debug
          deriving (Eq,Read,Show)

-- Save the model
u = recordType def (Proxy::Proxy Message)

t = main

main = do
  -- The subject of the discussion
  let subjL = ["Haskell"]
  let subj = Subject subjL

  putStrLn $ unwords ["Current Subject:",prettySubject (Subject []) subj,"\n"]

  -- get program options
  opts <- map read <$> getArgs

  let has = (`elem` (if null opts then [Input,Output] else opts))
  let on mode val = if has mode then [val] else []

  -- See raw messages being sent and received
  when (has Debug) $ logLevel DEBUG

  -- Start tasks
  void $ (mapM (\f -> async $ f subj) $ on Input inputMode ++ on Output outputMode) >>= waitAnyCancel
  where

    -- In a real chat system, it would be wasteful to receive messages about every subject
    -- To filter them at the source, to get only messages about
    -- the subject of our interest and its sub subjects:
    -- bySubject :: Pattern .. -> Pattern ..
    --let bySubject = $(filterPatternQ [p|Message _ (Subject subj) _|])
    -- runClient def (byPattern (bySubject (prefixPattern subjL))) $ \conn -> do
    -- NOTE: this is not implemented yet

     outputMode subj = runClient def ByType $ \conn ->
       -- Asynchronously, receive messages and display them
       -- We use a simple pipe, to get a message from the connection (pipeIn) and print it
        void $ execStateT (runEffect $ pipeIn conn >-> niceMessage subj >-> for cat (liftIO . putStrLn)) (ChatState False)

     inputMode subj = runClient def ByType $ \conn -> do
       user <- getName

       putStrLn $ unlines [""
                      ,"Help:"
                      ,"To send a message: just enter it and press return."
                      ,"To exit: Ctrl-D." --,"To exit: enter . (a single full stop)"
                      ]

       let msg = Message (User user) subj
       -- We can use two different systems to exchange messages:
       -- Either we use send/receive, as in:
       let sendOne = void . output conn . msg
       -- Or we use pipes with pipeIn/pipeOut (see the beautiful Pipes package for more info: http://hackage.haskell.org/package/pipes) as in:
       -- let sendOne c = runEffect $ yield (msg c) >-> pipeOut conn

       -- Let everybody know that we joined the discussion
       sendOne Join

       -- Then ask for recent messages
       sendOne AskHistory

       -- Read lines from the user (stdinLn) and send them out (pipeOut)
       runEffect $ for stdinLn (\txt -> unless (null txt) (yield . msg . TextMessage $ txt)) >-> pipeOut conn

       -- The user has had enough, time to say goodbye
       sendOne Leave

     getName = do
       putStrLn "Enter your name:"
       userName <- getLine
       if (length userName < 1)
         then getName
         else return userName

