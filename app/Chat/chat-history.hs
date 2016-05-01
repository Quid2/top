import           Chat.Model
import           Control.Monad.Trans.State
import           Network.Quid2

{-
 A simple bot that stores all messages it sees and returns them on request.

 TODOs:
 Return also messages relative to sub subjects.
 Limit number of messages stored.
 Store only relevant messages.
 Persist messages
 ..
-}

t = main

-- Non-persistent message store
type HistoryM = StateT [Message] IO

main = do
  -- Display messages sent and received
  logLevel DEBUG

  runClientForever def ByType $ \conn ->
    execStateT (runEffect $ pipeIn conn >-> historyAgent >-> pipeOut conn) []

   where

     historyAgent = do
       msg <- await
       -- Store all messages
       lift $ addMsg msg
       case content msg of
          -- On request, send a list of recent messages
          AskHistory -> do
            msgs <- lift $ getMsgsBySubject (subject msg)
            yield $ Message me (subject msg) (History msgs)
          _ -> return ()
       historyAgent

     me = User "hchat-history"

     addMsg msg = modify (msg:)
     getMsgsBySubject subj = gets (take 50 . filter (\msg -> subject msg == subj))

