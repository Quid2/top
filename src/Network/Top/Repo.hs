{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}
-- |Permanently register and retrieve absolute type definitions
module Network.Top.Repo(recordType
                        --,solveProxy
                       ,solveType
                       ,knownTypes) where
import           Data.Either.Extra
import           Data.List         (nub)
import qualified Data.Map          as M
import           Data.Maybe
import           ZM
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           System.Timeout

-- |Permanently record an absolute type definition
recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = runClient cfg ByType $ \conn -> mapM_ (output conn . Record) . absADTs $ proxy

-- |Retrieve all known types
knownTypes :: Config -> IO (Either String [(AbsRef, AbsADT)])
knownTypes cfg = runClient cfg ByType $ \conn -> do
  output conn AskDataTypes

  let loop = do
        msg <- input conn
        case msg of
          KnownDataTypes ts -> return ts
          _                 -> loop

  withTimeout 30 loop

-- |Retrieve the full type model for the given absolute type
solveType :: Repo -> Config -> AbsType -> IO (Either RepoError AbsTypeModel)
solveType repo cfg t = ((\env -> TypeModel t (M.fromList env)) <$>) <$> solveType_ repo cfg t
  where
    solveType_ :: Repo -> Config -> AbsType -> IO (Either RepoError [(AbsRef,AbsADT)])
    -- solveType_ repo cfg t = (solveRefsRec repo (resolveRef cfg )) (references t)
    solveType_ repo cfg t = runClient cfg ByType $ \conn -> (solveRefsRec repo (resolveRef__ conn)) (references t)

    solveRefsRec :: Repo -> RefSolver -> [AbsRef] -> IO (Either RepoError [(AbsRef,AbsADT)])
    solveRefsRec repo solver [] = return $ Right []
    solveRefsRec repo solver refs = do
      er <- allErrs <$> mapM (solveRef repo solver) refs
      case er of
        Left err -> return $ Left err
        Right ros -> (nub . (ros ++) <$>) <$> solveRefsRec repo solver (concatMap (innerReferences . snd) ros)

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
        Just o  -> return $ Right o

resolveRef cfg ref = do
  er <- strictTry $ resolveRef_ cfg ref
  return $ case er of
    Left exp -> Left (show exp)
    Right er -> er

resolveRef_ cfg ref = runClient cfg ByType (flip resolveRef__ ref)

resolveRef__ conn ref = do
    output conn (Solve ref)

    let loop = do
          msg <- input conn
          case msg of
            -- Solved t r | t == typ -> return $ (\e -> AbsoluteType (M.fromList e) typ) <$> r
            Solved sref sadt | ref == sref && absRef sadt == sref -> return $ Right sadt
            _ -> loop

    -- BUG: this returns an exception that is not captured
    fromMaybe (Left "Timeout") <$> timeout (seconds 30) loop

withTimeout :: Int -> IO a -> IO (Either String a)
withTimeout secs op = maybe (Left "Timeout") Right <$> timeout (seconds secs) op

absADTs :: Model a => Proxy a -> [AbsADT]
absADTs = typeADTs . absTypeModel
