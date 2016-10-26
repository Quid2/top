{-# LANGUAGE ScopedTypeVariables #-}
module Network.Top.Repo(recordType,solveType,knownTypes) where
import           Data.Maybe
import           Data.Typed
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           System.Timeout
import qualified Data.Text as T

t = t1 >> t2
   -- record (Proxy::Proxy Repo)
   -- solve (Proxy::Proxy Repo)
   -- let local = def {ip="127.0.0.1",port=8080}

t1 = recordType def (Proxy::Proxy (ChannelSelectionResult (WebSocketAddress IP4Address))) -- Repo)
t2 = solveType def (Proxy::Proxy Char)
t3 = knownTypes def
-- absADTs :: Model a => Proxy a -> [AbsADT]
z = mapM (putStrLn . prettyShow) $ absADTs (Proxy::Proxy Word32) --- Identifier) -- Repo)
g = recordType def (Proxy::Proxy Word32) --- Identifier) -- Repo))

recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = runClient cfg ByType $ \conn -> mapM_ (output conn . Record) . absADTs $ proxy

solveType :: Model a => Config -> Proxy a -> IO (Either T.Text [(AbsRef, AbsADT)])
solveType cfg proxy = runClient cfg ByType $ \conn -> do

    let typ = absType proxy
    output conn (Solve typ)

    let loop = do
          msg <- input conn
          case msg of
            Solved t r | t == typ -> return r
            _ -> loop

    fromMaybe (Left $ T.pack "Timeout") <$> timeout (seconds 30) loop

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

