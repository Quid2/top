-- |Persistent on-disk implementation of a repository of ZM types
module Repo.Disk(dbRepo) where

import           Repo.DB
import qualified Repo.Types as R
import           ZM

dbRepo :: FilePath   -- ^Directory to store the repo (e.g. \"/tmp\")
       -> IO R.Repo  -- ^A new repository
dbRepo dir = do
  db <- openDB dir
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
