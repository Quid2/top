module Repo
  ( getAbsTypeModel
  ) where

import           Control.Monad
import qualified Data.Map      as M
import           Repo.Types
import           ZM

{- |
Retrieve the full type model for the given absolute type, using the given Repo.

$setup
>>> import ZM
>>> import Data.Word
>>> import Repo.Memory(memRepo)
>>> import Repo.Auto(autoRepo)
>>> repo <- memRepo >>= autoRepo
>>> tst p = ((absTypeModel p ==) <$>) <$> (getAbsTypeModel repo (absType p))

>>> (((==) 3) . length . typeEnv <$>) <$> getAbsTypeModel repo (absType (Proxy::Proxy ((),Bool)))
Right True

>>> tst (Proxy :: Proxy (Bool,[Char],Word8))
Right True
-}
getAbsTypeModel :: Repo -> AbsType -> IO (Either String AbsTypeModel)
getAbsTypeModel repo t =
  (TypeModel t . M.fromList <$>) <$>
  addAbsRefsRec repo (Right []) (references t)

addAbsRefsRec ::
     Repo
  -> Either String [(AbsRef, AbsADT)]
  -> [AbsRef]
  -> IO (Either String [(AbsRef, AbsADT)])
addAbsRefsRec repo =
  foldM
    (\er ref ->
       case er of
         Left e   -> return $ Left e
         Right rs -> addAbsRefRec repo rs ref)

addAbsRefRec ::
     Repo
  -> [(AbsRef, AbsADT)]
  -> AbsRef
  -> IO (Either String [(AbsRef, AbsADT)])
addAbsRefRec repo rs ref = do
  eadt <- get repo ref
  case eadt of
    Nothing -> return $ Left $ "not found: " ++ show ref
    Just adt ->
      addAbsRefsRec repo (Right $ (ref, adt) : rs) (innerReferences adt)
-- getAbsRef ::
--     Repo
--  -> AbsRef
--  -> Connection (Function (Solve AbsRef AbsADT) AbsADT)
--  -> IO (Either String AbsADT)
-- getAbsRef repo ref conn = do
--  rr <- get repo ref
--  case rr of
--    Nothing -> funCall (Solve ref) conn >>= mapM (\o -> put repo o >> return o)
--    Just o -> return $ Right o
-- getFromRepo repo k = do
--   r <- R.get repo k
--   return $
--     case r of
--       Nothing  -> Left $ "Not found type " ++ killall
--       Just adt -> Right adt
