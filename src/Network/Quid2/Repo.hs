{-# LANGUAGE ScopedTypeVariables #-}
module Network.Quid2.Repo(recordType,solveType,knownTypes) where
import           Data.Maybe
import           Data.Typed
import           Network.Quid2.Run
import           Network.Quid2.Types
import           Network.Quid2.Util
import           Repo.Types
import           System.Timeout

t = t1 >> t2
   -- record (Proxy::Proxy Repo)
   -- solve (Proxy::Proxy Repo)
   -- let local = def {ip="127.0.0.1",port=8080}

t1 = recordType def (Proxy::Proxy Repo)
t2 = solveType def (Proxy::Proxy Char)
t3 = knownTypes def

recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = runClient cfg ByType $ \conn -> mapM_ (output conn . Record) . absADTs $ proxy

solveType :: Model a => Config -> Proxy a -> IO (Either String [(AbsRef, AbsADT)])
solveType cfg proxy = runClient cfg ByType $ \conn -> do

    let typ = absType proxy
    output conn (Solve typ)

    let loop = do
          msg <- input conn
          case msg of
            Solved t r | t == typ -> return r
            _ -> loop

    fromMaybe (Left "Timeout") <$> timeout (seconds 30) loop

knownTypes :: Config -> IO (Either String [(AbsRef, AbsADT)])
knownTypes cfg = runClient cfg ByType $ \conn -> do
  output conn AskDataTypes

  let loop = do
        msg <- input conn
        case msg of
          KnownDataTypes ts -> return ts
          _ -> loop

  withTimeout 30 loop

withTimeout :: Int -> IO a -> IO (Either String a)
withTimeout secs op = maybe (Left "Timeout") Right <$> timeout (seconds secs) op

