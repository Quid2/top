{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- |Permanently register and retrieve absolute type definitions
module Network.Top.Repo (RepoProtocol(..),recordType, solveType, knownTypes) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Either.Extra (lefts, rights)
import           Data.List         (nub)
import qualified Data.Map          as M
import           Data.Maybe
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           ZM

{-|
A (simplistic) protocol to permanently store and retrieve ADT definitions.
-}
data RepoProtocol = Record AbsADT                      -- ^Permanently record an absolute type
                  | Solve AbsRef                       -- ^Retrieve the absolute type
                  | Solved AbsRef AbsADT               -- ^Return the absolute type identified by an absolute reference
                  | AskDataTypes                       -- ^Request the list of all known data types
                  | KnownDataTypes [(AbsRef, AbsADT)]  -- ^Return the list of all known data types
  deriving (Eq, Ord, Show, Generic, Flat, Model)

--instance Flat [(AbsRef,AbsADT)]

type RefSolver = AbsRef -> IO (Either RepoError AbsADT)
type TypeSolver = AbsType -> IO (Either RepoError AbsTypeModel)
type RepoError = String -- SomeException

-- |Permanently record an absolute type definition
--
-- >> recordType def (Proxy :: Proxy Bool)
recordType :: Model a => Config -> Proxy a -> IO ()
recordType cfg proxy = runApp cfg ByType $ \conn -> mapM_ (output conn . Record) . absADTs $ proxy

-- |Retrieve all known types
--
-- >> knownTypes def
knownTypes :: Config -> IO (Either String [(AbsRef, AbsADT)])
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
solveType repo cfg t = ((TypeModel t . M.fromList) <$>) <$> solveType_ repo cfg t
  where
    solveType_ :: Repo -> Config -> AbsType -> IO (Either RepoError [(AbsRef,AbsADT)])
    solveType_ repo cfg t = runApp cfg ByType $ \conn -> (solveRefsRec repo (resolveRef__ conn)) (references t)

    solveRefsRec :: Repo -> RefSolver -> [AbsRef] -> IO (Either RepoError [(AbsRef,AbsADT)])
    solveRefsRec _     _     [] = return $ Right []
    solveRefsRec repo solver refs = do
      er <- allErrs <$> mapM (solveRef repo solver) refs
      case er of
        Left err -> return $ Left err
        Right ros -> (nub . (ros ++) <$>) <$> solveRefsRec repo solver (concatMap (innerReferences . snd) ros)

    allErrs :: [Either String r] -> Either String [r]
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

resolveRef :: Config -> AbsRef -> IO (Either String AbsADT)
resolveRef cfg ref = checked $ resolveRef_ cfg ref

resolveRef_ cfg ref = runApp cfg ByType (flip resolveRef__ ref)

resolveRef__ :: Connection RepoProtocol -> AbsRef -> IO (Either String AbsADT)
resolveRef__ conn ref = checked $ resolveRef___ conn ref

checked f = do
  eer <- strictTry f
  return $ case eer of
    Left exp -> Left (show exp)
    Right er -> er

resolveRef___ :: Connection RepoProtocol -> AbsRef -> IO (Either String AbsADT)
resolveRef___ conn ref = do
    output conn (Solve ref)

    let loop = do
          msg <- input conn
          case msg of
            -- Solved t r | t == typ -> return $ (\e -> AbsoluteType (M.fromList e) typ) <$> r
            Solved sref sadt | ref == sref && absRef sadt == sref -> return $ Right sadt
            _ -> loop

    -- BUG: this returns an exception that is not captured
    --fromMaybe (Left "Timeout") <$> withTimeout 25 loop
    join <$> withTimeout 25 loop

absADTs :: Model a => Proxy a -> [AbsADT]
absADTs = typeADTs . absTypeModel
