{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE TupleSections     #-}
module Data.Pattern.Matcher (
    MapTypeMatcher,
    typeMatcherMap,
    BTree(..),
    PatternMatcher,
    Match(..),
    patternMatcher,
    ) where

import qualified Data.Map             as M
import           Data.Maybe
import           Data.Pattern
import           ZM                   hiding (Name, Var)

--type PatternMatcher = (TypeMatchers,[Match AbsRef [BitMask]])

-- |A matcher for a pattern of a given type
type PatternMatcher = (MapTypeMatcher, Pattern)

-- |A map of types to the corresponding wildcard matcher
type MapTypeMatcher = M.Map AbsType BTree

-- |A form of constructors tree suitable for quick pattern matching
-- |Corresponds to a wildcard, it matches any value of a given type
data BTree = BFork BTree BTree -- ^ Constructors' tree
           | BCon [AbsType]    -- ^ Constructor parameters
           | Skip Int          -- ^ Skip a number of bits
           deriving (Show,Eq)

-- Convert a Pattern to a Matcher
patternMatcher :: AbsTypeModel -> Pattern -> PatternMatcher
patternMatcher at pat = (typeMatcherMap at,pat)

typeMatcherMap :: AbsTypeModel -> MapTypeMatcher
typeMatcherMap tm =
  let env = typeTree tm
  in M.mapWithKey (\t _ -> simplifyBTree $ typeTr env t) env
    where
      typeTr :: MapTypeTree -> AbsType -> BTree
      typeTr = typeTree_ BFork (\_ _ _ ps -> BCon ps)

      typeTree_ f c e t = conTree_ f c t [] (solve t e)

      conTree_ f c t bs (ConTree l r) = f (conTree_ f c t (0:bs) l) (conTree_ f c t (1:bs) r)
      conTree_ f c t bs (Con cn cs) = c t (convert cn :: String) (reverse bs) (fieldsTypes cs)
      -- conTree_ f c e t bs (Con cn cs) = c t (L.toString cn) (reverse bs) (map (typeTree_ f c e) (fieldsTypes cs))

-- |Simplify BTree substituting, whenever possible, an explicit traverse of the constructor tree
-- with the skip of a certain number of bits
-- For example the constructor tree (False | True) can be matched by: Skip 1 (bit).
simplifyBTree :: BTree -> BTree
simplifyBTree = brec sym
 where
   -- |Bottom-up tree simplification
   brec :: (BTree -> Maybe BTree) -> BTree -> BTree
   brec f n@(BFork l r) =
     fromMaybe (
     let n = BFork (brec f l) (brec f r)
     in fromMaybe n (f n)
     ) (f n)

   brec f n = fromMaybe n (f n)

   -- |Simplify constructor trees of the same length to a Skip
   sym :: BTree -> Maybe BTree
   sym (BFork (BCon []) (BCon [])) = Just $ Skip 1
   sym (BFork (Skip n1) (Skip n2)) | n1 == n2 = Just $ Skip (n1+1)
   sym _                           = Nothing

-- sl (BFork (BCon []) (BCon [])) = Just 1
-- sl (BFork l r)                 =( (,) <$> sl l <*> sl r) >>= snxt
-- snxt (n1,n2) | n1 == n2 = Just (n1+1)
-- snxt _       = Nothing


