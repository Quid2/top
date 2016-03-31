-- Pipes utilities
module Network.Quid2.Pipes(
  --module Pipes
   runEffect,(>->),yield,for,await,lift
  ,pipeIn,pipeOut
  ) where

import Network.Quid2.Types
import Network.Quid2.Run
import Pipes

-- |Produce values from a typed connection
pipeIn :: (Show a, MonadIO m,Flat a) => Connection a -> Producer a m ()
pipeIn conn = loop
     where
       loop = do
         v <- liftIO $ receive conn
         yield v
         loop

-- |Consume values into a typed connection
pipeOut :: (Show a, MonadIO m,Flat a) => Connection a -> Consumer a m ()
pipeOut conn = loop
     where
       loop = do
         v <- await
         liftIO $ send conn v
         loop
