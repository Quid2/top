module Repo.Auto where

import           Control.Concurrent.Async
import           Network.Top.Function
import           Network.Top.Repo         (knownTypes)
import           Network.Top.Run
import           Network.Top.Util
import qualified Repo.Types               as R
import           ZM.Type.Repo

{- |
Self-updating repo containing all currently known ADTs and that keeps itself up to date

$setup
>>> import ZM
>>> import Repo.Memory(memRepo)
>>> r <- memRepo >>= autoRepo
>>> TypeCon boolRef = absType (Proxy::Proxy Bool)

>>> R.get r boolRef
Just ...
-}
autoRepo repo = do
  recordThread <- funServe (recordADT repo)
  ets <- run knownTypes
  case ets of
    Right ts -> mapM_ (R.put repo . snd) ts
    Left e   -> warn ["Unable to get the list ok know types", show e]
  return
    R.Repo
      { R.get = R.get repo
      , R.put = R.put repo
      , R.close =
          do cancel recordThread
             R.close repo
      }
  where
    recordADT repo (Record adt) = R.put repo adt
