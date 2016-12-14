module Repo.Disk where
import qualified Data.Map   as M
import qualified Repo.Types as R
import Data.IORef
import Data.Typed
import Repo.DB

dbRepo dir = do
  db <- openDB dir
  --dbgS (stateDir cfg)
  return $ R.Repo {
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
