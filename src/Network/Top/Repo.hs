{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Network.Top.Repo(recordType
                       ,solveProxy,solveType
                       ,knownTypes) where
import qualified Data.Map          as M
import           Data.Maybe
import qualified Data.Text         as T
import           Data.Typed
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           System.Timeout
import Data.Either.Extra
import Data.List(nub)
-- import Data.Foldable(toList)
-- import Network
-- import Data.List
import Control.Monad

-- t = t1 >> t2
x = recordType def (Proxy::Proxy RepoProtocol)
-- y = recordType def (Proxy::Proxy AbsADT)
   -- solve (Proxy::Proxy Repo)
   -- let local = def {ip="127.0.0.1",port=8080}

t1 = recordType def (Proxy::Proxy (ChannelSelectionResult (WebSocketAddress IP4Address))) -- Repo)
-- t2 = solveType def (Proxy::Proxy Char)

t3 = knownTypes def
-- absADTs :: Model a => Proxy a -> [AbsADT]
z = mapM (putStrLn . prettyShow) $ absADTs (Proxy::Proxy Word32) --- Identifier) -- Repo)
g = recordType def (Proxy::Proxy Word32) --- Identifier) -- Repo))

recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = runClient cfg ByType $ \conn -> mapM_ (output conn . Record) . absADTs $ proxy

solveProxy :: Model a => Repo -> Config -> Proxy a -> IO (Either RepoError [(AbsRef,AbsADT)])
solveProxy repo cfg = solveType repo cfg . absType

solveType :: Repo -> Config -> AbsType -> IO (Either RepoError [(AbsRef,AbsADT)])
solveType repo cfg t = (solveRefsRec repo (resolveRef cfg )) (references t)

solveRefsRec :: Repo -> RefSolver -> [AbsRef] -> IO (Either RepoError [(AbsRef,AbsADT)])
solveRefsRec repo solver [] = return $ Right []
solveRefsRec repo solver refs = do
  er <- allErrs <$> mapM (solveRef repo solver) refs
  case er of
    Left err -> return $ Left err
    Right ros -> (nub . (ros ++) <$>) <$> solveRefsRec repo solver (concatMap (innerReferences . snd) ros)

-- solveRefs :: Repo -> RefSolver -> [AbsRef] -> IO (Either RepoError [(AbsRef,AbsADT)])
-- solveRefs repo solver refs = allErrs <$> mapM (solveRef repo solver) refs

allErrs :: [Either String r] -> Either String [r]
allErrs rs =
  let errs = filter isLeft rs
  in if null errs
     then sequence rs
     else Left (unlines $ map fromLeft errs)

solveRef :: Repo -> RefSolver -> AbsRef -> IO (Either RepoError (AbsRef,AbsADT))
solveRef repo solver ref = ((ref,) <$> )<$> do
  rr <- get repo ref
  case rr of
    Nothing -> solver ref >>= mapM (\o -> put repo o >> return o)
    Just o -> return $ Right o

resolveRef cfg ref = do
  er <- strictTry $ resolveRef_ cfg ref
  return $ case er of
    Left exp -> Left (show exp)
    Right er -> er

resolveRef_ cfg ref = runClient cfg ByType $ \conn -> do

    output conn (Solve ref)

    let loop = do
          msg <- input conn
          case msg of
            -- Solved t r | t == typ -> return $ (\e -> AbsoluteType (M.fromList e) typ) <$> r
            Solved sref sadt | ref == sref && refS sadt == sref -> return $ Right sadt
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

