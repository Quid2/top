{-# LANGUAGE DeriveFoldable    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections     #-}
module Data.Pattern.Types(
  Pattern(..),WildCard(..),showPatt
  ,patternQ,filterPatternQ,prefixPattern,onlyWildCards,HVar(..)
  ,Q,Pat,Match(..),pattern2Match,Matcher,envPattern,PatternMatcher,BitMask(..),andMask,MatchError(..),boolsSplit,bitSplit,mapPM,asMSBits,asLSBits,littleEndian16
  ) where

import Data.Bits
import           Data.Bifunctor
import qualified Data.ByteString            as B
import           Data.Either
import qualified Data.Flat.Bits             as V
import           Data.Foldable              (toList)
import           Data.List                  (intercalate)
import qualified Data.ListLike.String       as L
import           Data.Maybe
import qualified Data.Text                  as T
import           Data.Typed                 hiding (Con, Name, Val, Var)
import           Language.Haskell.TH        hiding (Match, Type)
import           Language.Haskell.TH.Syntax hiding (Match, Type)
{-
Alternative coding more? efficient to check for validity of values (not really)

5:
00
01
10
110
111
-}

-- Does the flat encoded info matches a pattern?
type Matcher = B.ByteString -> Bool

data MatchError = NotEnoughData | TooMuchData deriving (Eq,Show)

-- Low level matcher
data BitMask =
  Bits8 -- Match the indicated number of most significant bits with indicated value
  Int   -- numBits
  Word8 -- value has msb numBits set to expected valued, other bits are set to 0

  | Bits16 Int Word16
  | Bits32 Int Word32
  | Bits64 Int Word64

  -- | BVal BTree -- Constructor Tree
  deriving (Eq,Show)

  {- For alternative encoding
  NO: in any case we still need to distinguish among constructors to check what follows.
  | Skip Int -- numBits to skip, for num constructors = power of two
  | BCons
    Int -- numBits
    Int -- max value to skip numbits, if val is greater then skip numBits+1
  -}

-- 5 -> BFork (BFork BVal BVal) (BFork BVal (BFork BVal BVal))
-- data BTree = BFork BTree BTree | BCon [BTree] deriving (Show,Eq)

x = bitSplit [True,True,False,True] == [Bits8 4 208]
xx = bitSplit8 [True,True,False,True,False,False,False,True,True] == [Bits8 8 209,Bits8 1 128]

--type PatternMatcher = (TypeMatchers,[Match AbsRef [BitMask]])
type PatternMatcher = (TypeMatchers,[Match AbsRef [Bool]])

-- TODO: leave only needed types
envPattern :: AbsoluteType -> Pattern WildCard -> Either String PatternMatcher
-- envPattern at pat = second ((bitSplit8 <$>) <$>) . (matchTree at,) <$> pattern2Match at pat
envPattern at pat = (matchTree at,) <$> pattern2Match at pat

mapPM :: ([Bool] -> b) -> PatternMatcher -> (TypeMatchers,[Match AbsRef b])
mapPM f = second ((f <$>) <$>)

bitSplit8 [] = []
bitSplit8 bs =
  let (bs',bs'') = splitAt 8 bs
  in Bits8 (length bs') (asLSBits 8 bs') : bitSplit8 bs''

bitSplit [] = []
bitSplit bs | length bs <= 8   = [Bits8 (length bs) (asLSBits 8 bs)]
bitSplit bs | length bs <= 16  = [Bits16 (length bs) (asLSBits 16 bs)]
bitSplit bs | length bs <= 32  = [Bits32 (length bs) (asLSBits 32 bs)]
bitSplit bs | length bs <= 64  = [Bits64 (length bs) (asLSBits 64 bs)]
bitSplit bs =
  let (bs',bs'') = splitAt 64 bs
  in Bits64 64 (asLSBits 64 bs') : bitSplit bs''

boolsSplit :: Int -> [a] -> [[a]]
boolsSplit maxN bs | length bs <= maxN = [bs]
                   | otherwise = let (h,t) = splitAt maxN bs in h : boolsSplit maxN t

-- LSB
asLSBits :: Num c => Int -> [Bool] -> c
asLSBits n bs | n >= length bs = asNum bs

-- MSB
asMSBits :: Num c => Int -> [Bool] -> c
asMSBits n bs | n >= length bs = asNum (bs ++ replicate (n - length bs) False)

littleEndian16 w = (w .&. 255) * 256 + shiftR w 8

-- Little endian
-- asMSBitsLE n bs | n >= length bs && n `mod` 8 == 0 = asNum (bs ++ replicate (n - length bs) False)

y = map (andMask 8) [0..8]

m = asMSBits 16 [True,False,True]

-- n=1  -> 100000..
-- n=2  -> 11000..
andMask :: Num c => Int -> Int -> c
andMask tot n | tot>=n = asNum (replicate n True ++ replicate (tot-n) False)
              | otherwise = error $ unwords ["andMask",show tot, show n]
-- asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs
asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs

-- |A pattern matcher
data Match r bs = MatchBits bs    -- Match a flattened value
                | MatchType (Type r)  -- Match a wildcard of given type
  deriving (Show,Eq,Functor,Foldable,Traversable)

-- matchTypes :: [Match t] -> [Type t]
matchTypes = catMaybes . map matchType

matchType (MatchType t) = Just t
matchType _ = Nothing

-- |Optimise matcher
-- optMatch :: [Match t [Bool]] -> [Match t bs]
optMatch (MatchBits bs:MatchBits bs':t) = optMatch $ MatchBits (bs ++ bs'):t
optMatch (x:xs) = x : optMatch xs
optMatch [] = []

-- |A nested pattern
data Pattern v =
  -- |A constructor
  PCon
  T.Text         -- ^Name of the constructor (e.g. "True")
  [Pattern v]  -- ^Patterns for the parameters

  | PVar v      -- A variable

  -- This assumes a specific mapping of basic types to absolute types
  | PVal [Bool] -- A value, binary encoded (using 'flat')

  -- PInteger Integer -- TO BE MAPPED TO APPROPRIATE NUMERIC TYPE
  deriving (Functor,Foldable,Eq, Ord, Show, Generic)

instance Flat v => Flat (Pattern v)
instance Model v => Model (Pattern v)

-- |A Variable that can be either names (e.g. "a") or a wildcard "_"
data HVar = V String | W deriving (Eq, Ord, Show, Generic)
instance Flat HVar
instance Model HVar

-- |A wildcard "_", that matches any value
data WildCard = WildCard deriving (Eq, Ord, Show, Generic)
instance Flat WildCard
instance Model WildCard

prefixPattern :: (Foldable t, Flat a) => t a -> Pattern HVar
prefixPattern = listPatt (PVar W)

listPatt :: (Foldable t, Flat a) => Pattern v -> t a -> Pattern v
listPatt = foldr (\a p -> PCon "Cons" [valPattern a,p])

valPattern :: Flat a => a -> Pattern v
valPattern = PVal . V.bools

-- x = filter [p|\Message _ (subject:_) _ |]
-- \subject -> Con ... v1
filterPatternQ :: Quasi m => Q Pat -> m Exp
filterPatternQ patq = do
     p <- convertPattern (PVar . V) (PVar W) patq
      -- print $ pmatch p --
     let vars = map (\(V v) -> v) . filter isVar $ toList p
     -- TODO: when done, remove haskell-src-meta
     -- let Right c = parseExp $ concat["\\",unwords $ vars,"-> onlyWildCards <$> (",showPatt p,")"]
     -- let c = concat["\\",unwords $ toList p,"->",showPatt p]
     let c = LamE (map (VarP . mkName) vars) (UInfixE (VarE (mkName "onlyWildCards")) (VarE (mkName "<$>")) (ParensE (asExp p)))
     -- print c >> print c2 >> print (c == c2)
     return c

isVar (V _) = True
isVar _ = False

patternQ :: Quasi m => Q Pat -> m (Pattern WildCard)
patternQ = convertPattern (\n -> error $ unwords ["Variables are not allowed in patterns, use wildcards (_) only, found:",n]) (PVar WildCard)

-- Literals are converted to their flat representation (alternative: use proper definition?)
-- Anything else as a nested named pattern
-- convertPattern :: Quasi m => Q Pat -> m (Pattern String)
convertPattern
  :: Quasi m =>
     (String -> Pattern v) -> Pattern v -> Q Pat -> m (Pattern v)
convertPattern onVar onWild p = runQ (p >>= convertM onVar onWild)
  where
    convertM onVar onWild pat = case pat of
      ConP n [] | name n == "[]" -> return $ PCon "Nil" []
      ConP n args -> PCon (T.pack $ name n) <$> mapM (convertM onVar onWild) args
      VarP n -> return $ onVar (name n)
      WildP -> return onWild
      LitP l -> return . convLit $ l
      ParensP p -> convertM onVar onWild p
      InfixP p1 (Name (OccName ":" ) (NameG DataName (PkgName "ghc-prim") (ModName "GHC.Types"))) p2 -> (\a b -> PCon "Cons" [a,b]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2)
      ListP ps -> convList <$> mapM (convertM onVar onWild) ps
      TupP [p1,p2] -> (\a b -> PCon "Tuple2" [a,b]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2)
      TupP [p1,p2,p3] -> (\a b c -> PCon "Tuple3" [a,b,c]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2) <*> (convertM onVar onWild p3)
      TupP [p1,p2,p3,p4] -> (\a b c d -> PCon "Tuple4" [a,b,c,d]) <$> (convertM onVar onWild p1) <*> (convertM onVar onWild p2) <*> (convertM onVar onWild p3) <*> (convertM onVar onWild p4)
      p -> error . unwords $ ["Unsupported pattern",show p] -- pprint p,show p]

    name (Name (OccName n) _) = n

    convLit l = case l of
       CharL c -> valPattern c
       StringL s -> valPattern s
       -- BUG::always interpreted as Integral (signed Int) (should be mapped to right numerical type)
       IntegerL i -> valPattern i
       -- RationalL r -> valPattern r

    convList [] = PCon "Nil" []
    convList (h:t) = PCon "Cons" [h,convList t]

showPatt :: Pattern HVar -> String
showPatt (PCon n ps) = unwords ["Data.Pattern.Con",show n,"[",intercalate "," . map showPatt $ ps,"]"]
showPatt (PVar (V v)) = v -- concat ["val (",v,")"] -- showVar v
 --showPatt (Var W) = "Var W" -- "WildCard" -- "WildCard" -- "_"
showPatt p = show p -- show bs -- concat [Data.BitVector,show bs

asExp (PCon n ps) = AppE (AppE (c "Data.Pattern.Con") (LitE (StringL . T.unpack $ n))) (ListE (map asExp ps))
asExp (PVar (V v)) = VarE (mkName v)
asExp (PVar W) = AppE (c "Data.Pattern.Var") (c "W")

c = ConE . mkName

onlyWildCards W = WildCard

-- Convert a Pattern to a Matcher

pattern2Match :: AbsoluteType -> Pattern WildCard -> Either String [Match AbsRef [Bool]]
pattern2Match (AbsoluteType e t) pat = errs $ convert pat t
  where
    convert (PCon n ps) t =
        let adt = solvedADT e t
        --in case consIn (T.unpack n) adt of
        in case consIn (fromText n) adt of
          Nothing -> [Left $ unwords ["Constructor '"++T.unpack n++"' not present in",prettyShow t]]
          Just (bs,ts) -> Right (MatchBits bs) : concatMap (uncurry convert) (zip ps ts)
    -- convert (Var WildCard) t = [Right $ MatchType $ solveF mdls t]
    convert (PVar WildCard) t = [Right $ MatchType t]
    convert (PVal bs) _ = [Right $ MatchBits bs]

    errs r = if null (lefts r)
             then Right . optMatch . rights $ r
             else Left (unlines $ lefts r)

fromText = L.fromString . T.unpack
