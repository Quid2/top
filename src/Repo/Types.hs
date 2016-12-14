{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric ,DeriveAnyClass #-}
module Repo.Types where

import Data.Typed
import Data.Text(Text)
-- import Control.Exception

{-
A protocol to efficiently store and retrieve ADT definitions.

Requirement: Time and space efficient, exchange as little info as possible.
Requirement: anonymously verifiable?

Use cases:
Retrieve existing definitions:
AskAbsRefs
and then, for all unknown refs: AskADT

To record a data type definition:
-- Ask if is already known (AskIsKnown)
-- Wait for IsKnown Ref or timeout
PROB: need to wait for a timeout

Then/Or:
-- Send a KnownADT ref adt for the top definition
-- Answer queries AskADT with KnownADT 
-- Wait for a IsKnown AbsRef
PROB: need to send full def everytime

 wait for further queries for referred.
-}
-- data RepoProt =
--     AskIsKnown AbsRef -- Ask if the referred ADT is fully known
--   | IsKnown AbsRef -- Referred ADT is fully known (including all ADTs referred to)

--   | AskADT AbsRef -- Ask for adt with given ref
--   | KnownADT AbsRef AbsADT -- Return top level definition for ref

--   | AskRefs -- Ask for refs of all known types
--   | KnownRefs [(AbsRef,Identifier)] -- Return a known refs and their names
--   deriving (Eq, Ord, Show, Generic, Flat, Model)

-- PROB: slow for large data type or large number of types
data RepoProtocol = Record AbsADT
                  | Solve AbsRef -- Type
                  -- Returns top level definition?
                  -- | Solved AbsRef (Either RepoError [(AbsRef,AbsADT)])
                  | Solved AbsRef AbsADT -- (Either RepoError [(AbsRef,AbsADT)])
                  | AskDataTypes
                  | KnownDataTypes [(AbsRef,AbsADT)]
                  deriving (Eq, Ord, Show, Generic, Flat, Model)

type RepoError = String -- SomeException -- String

--class Repo repo where
--get :: repo -> IO (Either RepoError (t r))
--   put :: r -> t r -> IO ()

--instance Repo Abs where

--type RepoError = Text
-- data RepoError = UnknownType deriving (Eq, Ord, Show, Generic)

-- data Call a = Call a CallBack
--             | Return CallBack a
-- type CallBack = [Word8]

data Repo = Repo {get::AbsRef -> IO (Maybe AbsADT)
                 ,put::AbsADT -> IO ()
                 ,close:: IO ()}

type RefSolver = AbsRef -> IO (Either RepoError AbsADT)

type TypeSolver = AbsType -> IO (Either RepoError AbsTypeModel)
