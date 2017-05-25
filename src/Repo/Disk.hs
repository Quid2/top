-- |Persistent on-disk implementation of a repository of absolute types
module Repo.Disk(dbRepo) where
import           Repo.DB
import qualified Repo.Types as R
import           ZM

dbRepo dir = do
  db <- openDB dir
  --dbgS (stateDir cfg)
  return R.Repo {
        R.get = \ref -> do
            mr <- getDB db ref
            --print (unwords ["get",show ref,show mr])
            --dbg ["get",show ref,show $ isJust mr]
            return mr
        ,R.put = \adt -> do
            --dbg ["put",prettyShow adt]
            putDB db (absRef adt) adt
        ,R.close = closeDB db
        }
