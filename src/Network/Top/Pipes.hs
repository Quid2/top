-- Pipes utilities
module Network.Top.Pipes(
  --module Pipes
   runEffect,(>->),yield,for,await,lift
  ,pipeIn,pipeOut
  ) where

import           Control.Monad
import           Network.Top.Run
import           Network.Top.Types
import           Pipes

-- |Receive values from a typed connection, terminate when connection is closed
pipeIn :: (Show a, MonadIO m,Flat a) => Connection a -> Producer a m ()
-- pipeIn conn = loop
--      where
--        loop = do
--          mv <- liftIO $ input conn
--          case mv of
--            Nothing -> return ()
--            Just v -> do
--              yield v
--              loop

-- |Send values on a typed connection, terminate when connection is closed
pipeOut :: (Show a, MonadIO m,Flat a) => Connection a -> Consumer a m ()
-- pipeOut conn = loop
--      where
--        loop = do
--          v <- await
--          alive <- liftIO $ output conn v
--          when alive loop

pipeIn conn = loop
       where
         loop = do
           v <- liftIO $ input conn
           yield v
           loop

pipeOut conn = loop
  where
    loop = do
      v <- await
      liftIO $ output conn v
      loop
