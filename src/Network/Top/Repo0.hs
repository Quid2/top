{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- |Permanently register and retrieve absolute type definitions
module Network.Top.Repo0
  ( RepoProtocol(..)
  , recordType
  , recordADTs
  , solveType
  , solveRefs
  , knownTypes
  , String
  ) where

--import Prelude hiding (String)   
--import qualified Prelude as H
import qualified ZM.Type.String as Z 
import           Control.Monad
import           Data.Either.Extra (lefts)
import           Data.List         (nub)
import qualified Data.Map          as M
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           ZM
import           ZM.Type.Repo0
import Data.Bifunctor

type RefSolver = AbsRef -> IO (Either RepoError AbsADT)
-- type TypeSolver = AbsType -> IO (Either RepoError AbsTypeModel)
type RepoError = String -- SomeException

-- |Permanently record all the ADT definitions referred by a type, with all their dependencies
recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = recordADTs cfg . absADTs $ proxy

-- |Permanently record a set of ADT definitions with all their dependencies
recordADTs :: Foldable t => Config -> t AbsADT -> IO ()
recordADTs cfg adts = runApp cfg ByType $ \conn -> mapM_ (output conn . Record) adts

-- |Retrieve all known data types names (absolute references and names)
knownTypes :: Config -> IO (Either RepoError [(AbsRef, AbsADT)])
knownTypes cfg = runApp cfg ByType $ \conn -> do
  output conn AskDataTypes

  let loop = do
        msg <- input conn
        case msg of
          KnownDataTypes ts -> return ts
          _                 -> loop

  withTimeout 30 loop

-- |Retrieve the full type model for the given absolute type
-- from Top's RepoProtocol channel, using the given Repo as a cache
solveType :: Repo -> Config -> AbsType -> IO (Either RepoError AbsTypeModel)
solveType repo cfg t = (TypeModel t <$>) <$> solveRefs repo cfg (references t)

-- |Solve ADT references recursively, returning all dependencies.
solveRefs :: Repo -> Config -> [AbsRef] -> IO (Either RepoError (M.Map AbsRef AbsADT))
solveRefs repo cfg refs = runApp cfg ByType $ \conn -> solveRefsRec repo (resolveRef__ conn) refs
  where
    solveRefsRec :: Repo -> RefSolver -> [AbsRef] -> IO (Either RepoError (M.Map AbsRef AbsADT))
    solveRefsRec _     _     [] = return $ Right M.empty
    solveRefsRec repo solver refs = do
      er <- allErrs <$> mapM (solveRef repo solver) refs
      case er of
        Left err -> return $ Left err
        Right ros -> (M.union (M.fromList ros) <$>) <$> solveRefsRec repo solver (concatMap (innerReferences . snd) ros)

    allErrs :: [Either RepoError r] -> Either RepoError [r]
    allErrs rs =
      let errs = lefts rs
      in if null errs
         then sequence rs
         else Left (unlines errs)

    solveRef :: Repo -> RefSolver -> AbsRef -> IO (Either RepoError (AbsRef,AbsADT))
    solveRef repo solver ref = ((ref,) <$> )<$> do
      rr <- get repo ref
      case rr of
        Nothing -> solver ref >>= mapM (\o -> put repo o >> return o)
        Just o  -> return $ Right o

resolveRef :: Config -> AbsRef -> IO (Either RepoError AbsADT)
resolveRef cfg ref = checked $ resolveRef_ cfg ref

resolveRef_ :: Config -> AbsRef -> IO (Either RepoError AbsADT)
resolveRef_ cfg ref = runApp cfg ByType (`resolveRef__` ref)

resolveRef__ :: Connection RepoProtocol -> AbsRef -> IO (Either RepoError AbsADT)
resolveRef__ conn ref = checked $ resolveRef___ conn ref

resolveRef___ :: Connection RepoProtocol -> AbsRef -> IO (Either RepoError AbsADT)
resolveRef___ conn ref = do
    output conn (Solve ref)

    let loop = do
          msg <- input conn
          case msg of
            -- Solved t r | t == typ -> return $ (\e -> AbsoluteType (M.fromList e) typ) <$> r
            Solved sref sadt | ref == sref && absRef sadt == sref -> return $ Right sadt
            _ -> loop

    join <$> withTimeout 25 loop

absADTs :: Model a => Proxy a -> [AbsADT]
absADTs = typeADTs . absTypeModel

checked :: NFData b => IO (Either RepoError b) -> IO (Either RepoError b)
checked f = either (Left . show) id <$> strictTry f
