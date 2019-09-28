{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TupleSections             #-}

-- |Permanently register and retrieve absolute type definitions
module Network.Top.Repo
    --RepoProtocol(..)
  ( recordType
  , recordADTs
  , getAbsTypeModel
  -- , solveType
  -- , solveRefs
  , knownTypes
  , knownTypesRefs
  , String
  ) where

import           Control.Monad
import           Control.Monad        (foldM)
import           Data.Bifunctor
import           Data.Either.Extra    (lefts)
import           Data.List            (nub)
import qualified Data.Map             as M
import           Network.Top.Function
import           Network.Top.Run
import           Network.Top.Types
import           Network.Top.Util
import           Repo.Types
import           ZM
import           ZM.Type.Repo

--import Prelude hiding (String)
--import qualified Prelude as H
import qualified ZM.Type.String       as Z

type RefSolver = AbsRef -> IO (Either RepoError AbsADT)

-- type TypeSolver = AbsType -> IO (Either RepoError AbsTypeModel)
type RepoError = String -- SomeException

type FF f r = Connection (Function f r) -> IO (Either String r)

type FFN f r = Connection (Function f r) -> IO (Either String [r])

{- |
Permanently record all the ADT definitions referred by a type, with all their dependencies

>>> run $ recordType (Proxy :: Proxy Bool)
Right [()]

>>> run $ recordType (Proxy :: Proxy [Bool])
Right [(),()]

>>> run $ recordType (Proxy :: Proxy ((),(),(),(),()))
Right [(),()]
-}
recordType :: (Model a) => Proxy a -> FFN (Record AbsADT) ()
recordType proxy = recordADTs (absADTs proxy)

recordADTs adts = funCalls (map Record adts)

{- |
Retrieve references of all known data types absolute references and names

>>> (("Char" `elem`) . map snd <$>) <$> run knownTypesRefs
Right True
-}
knownTypesRefs :: FF (AllKnown (AbsRef, String)) [(AbsRef, String)]
knownTypesRefs
  -- (map (second (\(Z.String s) -> s)) <$>) <$>
 = funCall AllKnown

{- |
Retrieve all known data types

>>> (("Char" `elem`) . map (convert . declName . snd) <$>) <$> run knownTypes
Right True

-}
knownTypes :: FF (AllKnown (AbsRef, AbsADT)) [(AbsRef, AbsADT)]
knownTypes = funCall AllKnown

{- |
Retrieve the full type model for the given absolute type, using the given Repo as a cache

$setup
>>> import Repo.Memory(memRepo)
>>> import Data.Word
>>> repo <- memRepo
>>> p = Proxy :: Proxy (Bool,[Char],Word8)

>>> ((absTypeModel p ==) <$>) <$> run (getAbsTypeModel repo (absType p))
Right True
-}
getAbsTypeModel ::
     Repo
  -> AbsType
  -> Connection (Function (Solve AbsRef AbsADT) AbsADT)
  -> IO (Either String AbsTypeModel)
getAbsTypeModel repo t conn =
  (TypeModel t . M.fromList <$>) <$>
  addAbsRefsRec repo (Right []) (references t) conn

addAbsRefsRec ::
     Repo
  -> Either String [(AbsRef, AbsADT)]
  -> [AbsRef]
  -> Connection (Function (Solve AbsRef AbsADT) AbsADT)
  -> IO (Either String [(AbsRef, AbsADT)])
addAbsRefsRec repo r rs conn =
  foldM
    (\er ref ->
       case er of
         Left e   -> return $ Left e
         Right rs -> addAbsRefRec repo rs ref conn)
    r
    rs

addAbsRefRec ::
     Repo
  -> [(AbsRef, AbsADT)]
  -> AbsRef
  -> Connection (Function (Solve AbsRef AbsADT) AbsADT)
  -> IO (Either String [(AbsRef, AbsADT)])
addAbsRefRec repo rs ref conn = do
  eadt <- getAbsRef repo ref conn
  case eadt of
    Left e -> return $ Left e
    Right adt ->
      addAbsRefsRec repo (Right $ (ref, adt) : rs) (innerReferences adt) conn

getAbsRef ::
     Repo
  -> AbsRef
  -> Connection (Function (Solve AbsRef AbsADT) AbsADT)
  -> IO (Either String AbsADT)
getAbsRef repo ref conn = do
  rr <- get repo ref
  case rr of
    Nothing -> funCall (Solve ref) conn >>= mapM (\o -> put repo o >> return o)
    Just o -> return $ Right o

absADTs :: Model a => Proxy a -> [AbsADT]
absADTs = typeADTs . absTypeModel

checked :: NFData b => IO (Either RepoError b) -> IO (Either RepoError b)
checked f = either (Left . show) id <$> strictTry f
