module Chat.Util where
import Chat.Model
import Pipes
import Control.Monad
import           Control.Monad.Trans.State
import           Data.Maybe
import           Data.List

type ChatM = StateT ChatState IO

data ChatState = ChatState {
  shownHistory::Bool -- Have we already shown history (previous messages)? This is set to false after the initial display to avoid redoing it again.
  }

-- Pretty-print all messages relative to the current subject or one of its sub-subjects
niceMessage :: Subject -> Pipe Message String ChatM ()
niceMessage subj = loop
   where
     loop = do
         msg <- await
         when (inSubject subj $ subject msg) $ do
           mmsg <- lift $ prettyMsg subj msg
           case mmsg of
              Just pr -> yield pr
              Nothing -> return ()
         loop

prettyMsg :: Subject -> Message -> ChatM (Maybe String)
prettyMsg topSubj msg =
  let header = return . Just . (concat [prettySubject topSubj (subject msg),userName (fromUser msg) ++": "] ++)
  in case content msg of
    TextMessage txt -> header txt
    Join ->  header "I just joined the discussion."
    Leave -> header "I just left the discussion."
    History msgs -> do -- BUG: should be returned as individual lines
      shown <- state (\s -> (shownHistory s,ChatState True))
      if shown
        then return Nothing
        else Just . unlines . catMaybes <$> (mapM (prettyMsg topSubj) . reverse $ msgs)
    _ -> return Nothing

prettySubject :: Subject -> Subject -> [Prelude.Char]
prettySubject (Subject topSubj) (Subject subj) =
  let s = fromMaybe subj $ stripPrefix topSubj subj
  in if null s
     then ""
     else intercalate "/" s --concat ["(",intercalate "/" s,") "]

inSubject :: Subject -> Subject -> Bool
inSubject (Subject topSubj) (Subject subj) = topSubj `isPrefixOf` subj

