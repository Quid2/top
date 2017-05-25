-- |Transient in-memory implementation of a repository of absolute types
module Repo.Memory(memRepo) where

import           Data.IORef
import qualified Data.Map   as M
import qualified Repo.Types as R
import           ZM

memRepo = do
  db <- newIORef M.empty
  return $ R.Repo {
    R.get = \ref -> do
        --dbg (unwords ["get",show ref])
        M.lookup ref <$> readIORef db
    ,R.put = \adt -> do
        --dbg (unwords ["put",prettyShow adt])
        modifyIORef db $ M.insert (absRef adt) adt
    ,R.close = return ()
    }

