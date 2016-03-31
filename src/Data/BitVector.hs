{-# LANGUAGE ScopedTypeVariables ,NoMonomorphismRestriction #-}
module Data.BitVector(BitVector,bits,bitVector,V.fromList,V.toList) where

import qualified Data.Vector.Unboxed as V
import qualified Data.ByteString.Lazy as L
-- import qualified Data.Bits as B
import Data.Bits
-- import Data.Proxy
import Data.Flat
-- import Data.Hashable
import Data.Word

-- data BitVector = BitVector (V.Vector Bool)
type BitVector = V.Vector Bool

bits :: Flat a => a -> [Bool]
bits = V.toList . bitVector

bitVector :: forall a. Flat a => a -> BitVector
bitVector v = let e = encoded . postAligned $ v
                  Right (PostAligned _ f) = decoded e :: Decoded (PostAligned a)
                  bs = bytes e
                  numBits = 8 * L.length bs - fillerLength f
              in V.generate (fromIntegral numBits) (\n -> let (bb,b) = n `divMod` 8 in testBit (L.index bs (fromIntegral bb)) (7-b))

x = bits (True,False,True,True,False,True,True,True,False)
y = flat (True,False,True,True,False,True,True,True,False)
-- Useless
-- instance Binary BitVector where
--   encode = encode . V.toList
--   decode = V.fromList <$> decode

-- instance HasModel a => HasModel (V.Vector a) where envType _ = envType (Proxy::Proxy (Q.List a))
-- instance (V.Unbox a,Hashable a) => Hashable (V.Vector a) where hashWithSalt s = hashWithSalt s . V.toList
