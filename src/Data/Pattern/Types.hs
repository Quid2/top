{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE TupleSections     #-}
module Data.Pattern.Types(
  Pattern(..),WildCard(..),showPatt
  ,HVar(..),valPattern,isVar
  ,Matcher
  ) where

import qualified Data.ByteString as B
import qualified Data.Flat.Bits  as V
import           Data.List       (intercalate)
import           Data.Typed      hiding (Con, Name, Val, Var)

-- |High-level pattern definition, used for Pattern channels
data Pattern v =
  -- |A constructor
  PCon
  String       -- ^Name of the constructor (e.g. "True")
  [Pattern v]  -- ^Patterns for the parameters

  | PVar v     -- A variable

  -- This assumes a specific mapping of basic types to absolute types
  | PVal [Bool] -- A value, binary encoded (using 'flat')

  -- PInteger Integer -- TO BE MAPPED TO APPROPRIATE NUMERIC TYPE
  deriving (Functor,Foldable,Eq, Ord, Show, Generic, Flat)

instance Model v => Model (Pattern v)

-- |A Variable that can be either a name (e.g. "a") or a wildcard "_"
data HVar = V String
          | W
  deriving (Eq, Ord, Show, Generic, Flat, Model)

isVar :: HVar -> Bool
isVar (V _) = True
isVar _     = False

-- |A wildcard "_", that matches any value
data WildCard = WildCard
  deriving (Eq, Ord, Show, Generic, Flat, Model)

prefixPattern :: (Foldable t, Flat a) => t a -> Pattern HVar
prefixPattern = listPatt (PVar W)

listPatt :: (Foldable t, Flat a) => Pattern v -> t a -> Pattern v
listPatt = foldr (\a p -> PCon "Cons" [valPattern a,p])

valPattern :: Flat a => a -> Pattern v
valPattern = PVal . V.bools

showPatt :: Pattern HVar -> String
showPatt (PCon n ps) = unwords ["Data.Pattern.Con",show n,"[",intercalate "," . map showPatt $ ps,"]"]
showPatt (PVar (V v)) = v -- concat ["val (",v,")"] -- showVar v
 --showPatt (Var W) = "Var W" -- "WildCard" -- "WildCard" -- "_"
showPatt p = show p -- show bs -- concat [Data.BitVector,show bs

onlyWildCards :: HVar -> WildCard
onlyWildCards W = WildCard

type Matcher = B.ByteString -> Bool
