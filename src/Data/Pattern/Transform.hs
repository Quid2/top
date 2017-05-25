{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
-- |Convert an Haskell pattern to the form accepted by ByPattern channels
module Data.Pattern.Transform (byPattern, byPattern_) where

import qualified Data.Flat.Bits       as V
import           Data.Int
import qualified Data.ListLike.String as L
import qualified Data.Map             as M
import           Data.Pattern.Types
import           Data.Pattern.Util
import           ZM           hiding (Name)
import           Data.Word

-- |Convert an Haskell pattern to the form accepted by ByPattern channels
-- or throw an error if conversion fails
byPattern :: forall a. Model a => Pat PRef -> ByPattern a
byPattern = either error id . byPattern_

-- |Convert an Haskell pattern to the form accepted by ByPattern channels
byPattern_ :: forall a. Model a => Pat PRef -> Either String (ByPattern a)
byPattern_ pat =
  let tm = absTypeModel (Proxy :: Proxy a)
      ctMap = typeTree tm
      solveCons t = let Just ct = M.lookup t ctMap in (ct,t)

      conv (PCon n ps) (ct,t) =
        case constructorInfo (L.fromString n) ct of
          Nothing -> err ["Constructor '"++ n ++"' not present in",show t]
          Just (bs,ts) | length ts == length ps -> Right (MatchValue . map boolToBit $ bs) : concatMap (uncurry conv) (zip ps $ map solveCons ts)
                       | otherwise -> err ["Constructor",n,"has",show (length ts),"parameters, found",show (length ps)]

      conv (PName (PInt i)) (_,t) | t==word8Type = val (fromIntegral i::Word8)
                                  | t==word16Type = val (fromIntegral i::Word16)
                                  | t==word32Type = val (fromIntegral i::Word32)
                                  | t==word64Type = val (fromIntegral i::Word64)
                                  | t==wordType = val (fromIntegral i::Word)
                                  | t==int8Type = val (fromIntegral i::Int8)
                                  | t==int16Type = val (fromIntegral i::Int16)
                                  | t==int32Type = val (fromIntegral i::Int32)
                                  | t==int64Type = val (fromIntegral i::Int64)
                                  | t==intType = val (fromIntegral i::Int)
                                  | t==integerType = val (fromIntegral i::Integer)
                                  | otherwise = terr t i

      conv (PName (PRat r)) (_,t)| t==floatType = val (fromRational r::Float)
                                 | t==doubleType = val (fromRational r::Double)
                                 | otherwise = terr t r

      conv (PName (PChar c)) (_,t) | t == charType = val c
                                   | otherwise = terr t c

      conv (PName (PString s)) (_,t) | t == stringType = val s
                                     | otherwise = terr t s

      conv (PName PWild) (_,t) = [Right $ MatchAny t]

      conv (PName (PVar v)) _ = err ["Variables are not allowed in patterns, use wildcards (_) only, found:",v]

      --conv p _ = error (show p)

  in ByPattern . optPattern <$> collectErrors (conv pat (solveCons (typeName tm)))

     where
       val a = [Right . MatchValue . map boolToBit . V.toBools . V.bits $ a]
       err ls = [Left . unwords $ ls]
       terr expType r = err ["Type mismatch: expected",show expType,"type, found",show r]

boolToBit False = V0
boolToBit True = V1
