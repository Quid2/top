-- |Access Top connections as Pipes
module Network.Top.Pipes (
  runPipe,
  pipeIn,
  pipeOut,

  -- *Re-exports from Pipe
  runEffect,
  (>->),
  yield,
  for,
  await,
  lift,
) where

import Flat
import Network.Top.Types
import Pipes

{- |
>>> import qualified Pipes.Prelude as P
>>> import Data.Time
>>> import Network.Top.Run
>>> run $ \conn -> pipeIn conn >-> P.take 3 >-> P.map (show :: Bool -> String) >-> P.drain
-}
runPipe :: (Show a, MonadIO m, Flat a) => Pipe a a m () -> Connection a -> m ()
runPipe agent conn = runEffect $ pipeIn conn >-> agent >-> pipeOut conn

-- |Receive values from a typed connection, terminate when connection is closed
pipeIn :: (Show a, MonadIO m, Flat a) => Connection a -> Producer a m ()

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
pipeOut :: (Show a, MonadIO m, Flat a) => Connection a -> Consumer a m ()

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
