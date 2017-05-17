{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Data.Pattern.Types (
    Matcher,
    Pat(..),
    PRef(..),
    Match(..),
    optPattern,
    Pattern,
    ByPattern(..),
    -- *Variables
    WildCard(..),
    ) where

import qualified Data.ByteString as B
-- import qualified Data.Flat.Bits  as V
import           Data.List       (intercalate)
import           ZM      hiding (Con, Name, Var)

--newtype Pattern = Pattern [Match] deriving (Show,Eq,Generic,Flat,Model)

-- |A router indexed by a pattern of a given type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type, that match the given pattern, sent by other agents
data ByPattern a = ByPattern Pattern
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ByPattern a)

type Pattern = [Match [Bool]]
instance Flat [Match [Bool]]

-- |A simple pattern match
data Match v = MatchValue v            -- ^Match a flattened value
             | MatchAny (Type AbsRef)  -- ^Match any value of the given type (wildcard)
  deriving (Show, Eq, Ord, Generic, Flat,Functor) -- ,Foldable,Traversable)

instance Model v => Model (Match v)

-- |Optimise pattern
optPattern :: [Match [a]] -> [Match [a]]
optPattern (MatchValue []:t) = optPattern t
optPattern (MatchValue bs:MatchValue bs':t) = optPattern $ MatchValue (bs ++ bs'):t
optPattern (x:xs) = x : optPattern xs
optPattern [] = []

-- |A matcher, a predicate defined over the binary representation of a value
type Matcher = B.ByteString -> Bool

-- |Pattern representation used for internal processing
data Pat v =
  -- |A constructor
  PCon
  String   -- Name of the constructor (e.g. "True")
  [Pat v]  -- Patterns for the parameters

  | PName v

  --deriving (Functor,Foldable,Eq, Ord, Show, Generic, Flat)
  deriving (Eq, Ord, Show, Generic, Flat)

-- instance Flat v => Flat [Pat v]
instance Model v => Model (Pat v)

-- |Representation of literals and variables as returned by the TH pattern parser.
data PRef = PInt Integer
          | PRat Rational
          | PChar Char
          | PString String
          | PWild
          | PVar String
  deriving (Eq, Ord, Show) -- , Generic, Flat, Model)

-- |A Variable that can be either a name (e.g. "a") or a wildcard "_"
data VarOrWild = V String
               | W
  deriving (Eq, Ord, Show, Generic, Flat, Model)

isVar :: VarOrWild -> Bool
isVar (V _) = True
isVar _     = False

-- |A wildcard "_", that matches any value
data WildCard = WildCard
  deriving (Eq, Ord, Show, Generic, Flat, Model)

onlyWildCards :: VarOrWild -> WildCard
onlyWildCards W = WildCard
onlyWildCards _ = error "Only wildcards (_) are allowed"

-- |Pattern _:_
-- prefixPattern :: (Foldable t, Flat a) => t a -> Pattern HVar
-- prefixPattern = listPatt (PVar W)

--listPatt :: (Foldable t, Flat a) => Pattern v -> t a -> Pattern v
--listPatt = foldr (\a p -> PCon "Cons" [valPattern a,p])

showPatt :: Pat VarOrWild -> String
showPatt (PCon n ps) = unwords ["Data.Pattern.Con",show n,"[",intercalate "," . map showPatt $ ps,"]"]
showPatt (PName (V v)) = v -- concat ["val (",v,")"] -- showVar v
 --showPatt (Var W) = "Var W" -- "WildCard" -- "WildCard" -- "_"
showPatt p = show p -- show bs -- concat [Data.BitVector,show bs


