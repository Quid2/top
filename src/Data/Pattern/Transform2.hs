{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ScopedTypeVariables       #-}
module Data.Pattern.Transform2(Pattern,ByPattern(..),byPattern) where

import           Data.Int
import qualified Data.Map           as M
import           Data.Pattern.Types
import           Data.Typed         hiding (Name)
import           Data.Word
import qualified Data.Flat.Bits       as V
import           Data.Pattern.Util

type Pattern = Pat (Either WildCard [Bool])

-- |A router index by a pattern of a given type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type, that match the given pattern, sent by other agents
data ByPattern a = ByPattern Pattern
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ByPattern a)

byPattern :: forall a. Model a => Pattern PRef -> ByPattern a
byPattern pat =
  let tm = absTypeModel (Proxy :: Proxy a)
      ctMap = typeTree tm
      solve t = let Just ct = M.lookup t ctMap in (ct,t)

      conv (PCon n ps) (ct,_) = PCon n (map (uncurry conv) $ zip ps (map solve . fieldsTypes . constrFields $ ct))

      -- BUG: INCOMPLETE
      conv (PName PWild) _ = PName (Left WildCard)

      conv (PName (PInt i)) (_,t) | t==word8Type = valPattern (fromIntegral i::Word8)
                                 | t==word16Type = valPattern (fromIntegral i::Word16)
                                 | t==word32Type = valPattern (fromIntegral i::Word32)
                                 | t==word64Type = valPattern (fromIntegral i::Word64)
                                 | t==wordType = valPattern (fromIntegral i::Word)
                                 | t==int8Type = valPattern (fromIntegral i::Int8)
                                 | t==int16Type = valPattern (fromIntegral i::Int16)
                                 | t==int32Type = valPattern (fromIntegral i::Int32)
                                 | t==int64Type = valPattern (fromIntegral i::Int64)
                                 | t==intType = valPattern (fromIntegral i::Int)
                                 | t==integerType = valPattern (fromIntegral i::Integer)

      conv (PName (PRat r)) (_,t)| t==floatType = valPattern (fromRational r::Float)
                                | t==doubleType = valPattern (fromRational r::Double)

  in ByPattern (conv pat (solve (typeName tm)))

-- |Pattern corresponding to a known value
valPattern :: Flat a => a -> Pattern (Either WildCard [Bool])
valPattern = PName . Right . V.bools
