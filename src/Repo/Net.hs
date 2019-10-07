-- | A Repo that retrieves and saves on the Top network
module Repo.Net where

import           Control.Concurrent.Async
import           Control.Monad
import           Network.Top.Function
import           Network.Top.Repo         (recordADT, solveAbsRef)
import           Network.Top.Run
import           Network.Top.Util
import qualified Repo.Types               as R
import           ZM.Type.Repo

{- |
$setup
>>> import ZM
>>> TypeCon boolRef = absType (Proxy::Proxy Bool)

>>> R.get netRepo boolRef
Just ...
-}
netRepo =
  R.Repo
    { R.get = \ref -> eitherToMaybe <$> run (solveAbsRef ref)
    , R.put = void . run . recordADT
    , R.close = return ()
    }
