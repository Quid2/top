{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PackageImports   #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
-- |A persistent repository for absolute types, based on acid-state
module Repo.DB(DBState(..),wholeDB,openDB,closeDB,getDB,putDB) where

import           "mtl" Control.Monad.Reader
import           "mtl" Control.Monad.State
import           Data.Acid
import qualified Data.Map             as M
import           Data.SafeCopy
import           Data.Typeable
import           System.FilePath
import           ZM

type DB = AcidState DBState

newtype DBState = DBState AbsEnv
             deriving (Typeable,Show)

-- Transactions
whole :: Query DBState DBState
whole = ask

insert :: AbsRef -> AbsADT -> Update DBState ()
insert key value = modify (\(DBState st) -> DBState (M.insert key value st))

getByRef :: AbsRef -> Query DBState (Maybe AbsADT)
getByRef key = asks (\(DBState st) -> M.lookup key st)

makeAcidic ''DBState ['whole,'insert,'getByRef]

-- API
wholeDB :: DB -> IO DBState
wholeDB db = query db Whole

emptyDB :: DBState
emptyDB = DBState M.empty

openDB :: FilePath -> IO DB
openDB dir = do
    db <- openLocalStateFrom (dbDir dir) emptyDB
    -- wholeDB db >>= print
    createCheckpoint db
    return db

getDB :: DB -> AbsRef -> IO (Maybe AbsADT)
getDB db k = query db (GetByRef k)

putDB :: DB -> AbsRef -> AbsADT -> IO ()
putDB db k v = update db (Insert k v)

closeDB :: AcidState st -> IO ()
closeDB = closeAcidState

-- Utilities

dbDir :: FilePath -> FilePath
dbDir dir = dir </> "ADTS"

$(deriveSafeCopy 0 'base ''Type)
$(deriveSafeCopy 0 'base ''Identifier)
$(deriveSafeCopy 0 'base ''UnicodeSymbol)
$(deriveSafeCopy 0 'base ''UnicodeLetter)
$(deriveSafeCopy 0 'base ''UnicodeLetterOrNumberOrLine)
$(deriveSafeCopy 0 'base ''SHA3_256_6)
$(deriveSafeCopy 0 'base ''SHAKE128_48)
$(deriveSafeCopy 0 'base ''AbsRef)
$(deriveSafeCopy 0 'base ''ConTree)
$(deriveSafeCopy 0 'base ''ADTRef)
$(deriveSafeCopy 0 'base ''ADT)
$(deriveSafeCopy 0 'base ''NonEmptyList)
$(deriveSafeCopy 0 'base ''DBState)
