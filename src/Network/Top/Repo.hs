{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |Permanently register and retrieve absolute type definitions
module Network.Top.Repo
--RepoProtocol(..)
  (
  recordType,
  recordADTs,
  recordADT,
  knownTypes,
  -- , knownTypesRefs
  solveAbsRef,
  solveAbsRefs,
  getAbsTypeModel,
  String,
) where

import Control.Monad (foldM)
import Data.Bifunctor
import Data.Either.Extra (lefts)
import Data.List (nub)
import qualified Data.Map as M
import Network.Top.Function
import Network.Top.Run
import Network.Top.Types
import Network.Top.Util
import Repo.Types (Repo (get, put))
import ZM (
  AbsADT,
  AbsRef,
  AbsType,
  AbsTypeModel,
  Model,
  NFData,
  Proxy,
  TypeModel (TypeModel),
  absTypeModel,
  innerReferences,
  references,
  typeADTs,
 )
import ZM.Type.Repo (AllKnown (..), Record (..), Solve (..))
--import Prelude hiding (String)
--import qualified Prelude as H
import qualified ZM.Type.String as Z

type RefSolver = AbsRef -> IO (Either RepoError AbsADT)

-- type TypeSolver = AbsType -> IO (Either RepoError AbsTypeModel)
type RepoError = String -- SomeException

type FF f r = Connection (Function f r) -> IO (Either String r)

type FFN f r = Connection (Function f r) -> IO (Either String [r])

{- |
Permanently record all the ADT definitions referred by a type, with all their dependencies

>>> import Data.Proxy
>>> run $ recordType (Proxy :: Proxy Bool)
Right [()]

>>> run $ recordType (Proxy :: Proxy [Bool])
Right [(),()]

>>> run $ recordType (Proxy :: Proxy ((),(),()))
Right [(),()]

>>> run $ recordType (Proxy :: Proxy ((),(),(),()))
Right [(),()]

>>> run $ recordType (Proxy :: Proxy ((),(),(),(),()))
Right [(),()]
-}
recordType :: (Model a) => Proxy a -> FFN (Record AbsADT) ()
recordType proxy = recordADTs (absADTs proxy)

recordADT :: AbsADT -> FF (Record AbsADT) ()
recordADT adt = funCall (Record adt)

recordADTs :: [AbsADT] -> FFN (Record AbsADT) ()
recordADTs adts = funCalls (map Record adts)

absADTs :: Model a => Proxy a -> [AbsADT]
absADTs = typeADTs . absTypeModel

{- |
Retrieve references of all known data types absolute references and names

>>> (("Char" `elem`) . map snd <$>) <$> run knownTypesRefs
Right True
-}
knownTypesRefs :: FF (AllKnown (AbsRef, String)) [(AbsRef, String)]
knownTypesRefs =
  -- (map (second (\(Z.String s) -> s)) <$>) <$>
  funCall AllKnown

{- |
Retrieve all known data types

>>> import ZM
>>> (("Char" `elem`) . map (convert . declName . snd) <$>) <$> run knownTypes
Right True
-}
knownTypes :: FF (AllKnown (AbsRef, AbsADT)) [(AbsRef, AbsADT)]
knownTypes = funCall AllKnown

-- knownTypes = ((M.fromList <$>) <$>) . funCall AllKnown

{- |
Retrieve an ADT by its absolute reference

\$setup
>>> import Data.Map
>>> import ZM
>>> [(boolRef,boolADT)] = toList $ absEnv (Proxy :: Proxy Bool)

>>> ((== boolADT) <$>) <$> run (solveAbsRef boolRef)
Right True
-}
solveAbsRef :: AbsRef -> FF (Solve AbsRef AbsADT) AbsADT
solveAbsRef ref = funCall (Solve ref)

solveAbsRefs :: [AbsRef] -> FFN (Solve AbsRef AbsADT) AbsADT
solveAbsRefs refs = funCalls (map Solve refs)

{-
Retrieve the full type model for the given absolute type, using the given Repo as a cache

$setup
>>> import Repo.Memory(memRepo)
>>> import Data.Word
>>> repo <- memRepo
>>> tst p = ((absTypeModel p ==) <$>) <$> run (getAbsTypeModel repo (absType p))

>>> tst (Proxy :: Proxy Bool)
Right True

>>> tst (Proxy :: Proxy (Bool,[Char],Word8))
Right True
-}
getAbsTypeModel ::
  Repo ->
  AbsType ->
  Connection (Function (Solve AbsRef AbsADT) AbsADT) ->
  IO (Either String AbsTypeModel)
getAbsTypeModel repo t conn =
  (TypeModel t . M.fromList <$>)
    <$> addAbsRefsRec repo (Right []) (references t) conn

addAbsRefsRec ::
  Repo ->
  Either String [(AbsRef, AbsADT)] ->
  [AbsRef] ->
  Connection (Function (Solve AbsRef AbsADT) AbsADT) ->
  IO (Either String [(AbsRef, AbsADT)])
addAbsRefsRec repo r rs conn =
  foldM
    ( \er ref ->
        case er of
          Left e -> return $ Left e
          Right rs -> addAbsRefRec repo rs ref conn
    )
    r
    rs

addAbsRefRec ::
  Repo ->
  [(AbsRef, AbsADT)] ->
  AbsRef ->
  Connection (Function (Solve AbsRef AbsADT) AbsADT) ->
  IO (Either String [(AbsRef, AbsADT)])
addAbsRefRec repo rs ref conn = do
  eadt <- getAbsRef repo ref conn
  case eadt of
    Left e -> return $ Left e
    Right adt ->
      addAbsRefsRec repo (Right $ (ref, adt) : rs) (innerReferences adt) conn

getAbsRef ::
  Repo ->
  AbsRef ->
  Connection (Function (Solve AbsRef AbsADT) AbsADT) ->
  IO (Either String AbsADT)
getAbsRef repo ref conn = do
  rr <- get repo ref
  case rr of
    Nothing -> funCall (Solve ref) conn >>= mapM (\o -> put repo o >> return o)
    Just o -> return $ Right o

checked :: NFData b => IO (Either RepoError b) -> IO (Either RepoError b)
checked f = either (Left . show) id <$> strictTry f
