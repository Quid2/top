{-# LANGUAGE DeriveGeneric #-}
module Repo.Types where

import Data.Typed
import Data.Text(Text)

data Repo = Record AbsADT
          | Solve AbsType
          | Solved AbsType (Either RepoError [(AbsRef,AbsADT)])
          | AskDataTypes
          | KnownDataTypes [(AbsRef,AbsADT)]
          deriving (Eq, Ord, Show, Generic)

type RepoError = Text
-- data RepoError = UnknownType deriving (Eq, Ord, Show, Generic)
-- instance Flat RepoError
-- instance Model RepoError

instance Flat Repo
instance Model Repo

-- data Call a = Call a CallBack
--             | Return CallBack a
-- type CallBack = [Word8]
