{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}

-- |Pattern related types
module Data.Pattern.Types (
    Matcher,

    -- *Top patterns
    ByPattern(..),
    Pattern,
    Match(..),
    Bit(..),
    optPattern,

    -- *Internal patterns
    IPattern,
    Pat(..),
    PRef(..),
    --WildCard(..)
    ) where

import qualified Data.ByteString as B
import           ZM              hiding (Con, Name, Var)
import           ZM.Type.Bit

-- |A matcher is a predicate defined over the binary representation of a value
type Matcher = B.ByteString -> Bool

{-|
A routing protocol specified by a pattern and a type.

Once a connection is established, clients:

   * can send messages of the given type

   * will receive all messages of the same type, that match the given pattern, sent by other agents
-}
newtype ByPattern a = ByPattern Pattern
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ByPattern a)

-- |A Pattern is just a list of matches, values are represented by their Flat binary serialisation
type Pattern = [Match [Bit]]
-- instance Flat [Match [Bit]]

-- |Match either a flattened value of any value of a given type
data Match v = MatchValue v            -- ^Match the specified value
             | MatchAny (Type AbsRef)  -- ^Match any value of the given type (wildcard)
  deriving (Show, Eq, Ord, Generic, Flat,Functor)

instance Model v => Model (Match v)

-- |Optimise a Pattern by concatenating adjacent value matches
optPattern :: Pattern -> Pattern
optPattern (MatchValue []:t) = optPattern t
optPattern (MatchValue bs:MatchValue bs':t) = optPattern $ MatchValue (bs ++ bs'):t
optPattern (x:xs) = x : optPattern xs
optPattern [] = []

-- |Internal pattern representation
type IPattern = Pat PRef

-- |Pattern representation used for internal processing
data Pat v =
  -- |A constructor
  PCon  {pConsName::String         -- ^Name of the constructor (e.g. "True")
        ,pConsParameters::[Pat v]  -- ^Constructor parameters
        }

  -- |A primitive value (for example `PRef`)
  | PName v

  deriving (Eq, Ord, Show)

--instance Model v => Model (Pat v)

-- |Literals and variables
data PRef = PInt Integer
          | PRat Rational
          | PChar Char
          | PString String
          | PWild
          | PVar String
  deriving (Eq, Ord, Show)

-- -- |A Variable that can be either a name (e.g. "a") or a wildcard "_"
-- data VarOrWild = V String
--                | W
--   deriving (Eq, Ord, Show, Generic, Flat, Model)
-- --
-- -- isVar :: VarOrWild -> Bool
-- isVar (V _) = True
-- isVar _     = False

-- -- |A wildcard "_", that matches any value
-- data WildCard = WildCard
--   deriving (Eq, Ord, Show, Generic, Flat, Model)

-- onlyWildCards :: VarOrWild -> WildCard
-- onlyWildCards W = WildCard
-- onlyWildCards _ = error "Only wildcards (_) are allowed"

-- List Pattern  _:_
-- prefixPattern :: (Foldable t, Flat a) => t a -> Pattern HVar
-- prefixPattern = listPatt (PVar W)

--listPatt :: (Foldable t, Flat a) => Pattern v -> t a -> Pattern v
--listPatt = foldr (\a p -> PCon "Cons" [valPattern a,p])

-- showPatt :: Pat VarOrWild -> String
-- showPatt (PCon n ps) = unwords ["Data.Pattern.Con",show n,"[",intercalate "," . map showPatt $ ps,"]"]
-- showPatt (PName (V v)) = v -- concat ["val (",v,")"] -- showVar v
--  --showPatt (Var W) = "Var W" -- "WildCard" -- "WildCard" -- "_"
-- showPatt p = show p -- show bs -- concat [Data.BitVector,show bs

