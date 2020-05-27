module Data.Pattern.Bits
  ( BitMask(..)
  , bitSplit
  , asMSBits
  , andMask
  , listSplit
  , mapPM
  )
where

import           Data.Bifunctor
import           Data.Bits
import           Data.Word
import           ZM.Type.Bit

-- data MatchError = NotEnoughData | TooMuchData deriving (Eq,Show)
-- |Low level pattern, for efficient matching
data BitMask
  = Bits8
      Int -- ^ numBits
      Word8 -- ^ value has its least significant numBits set to expected value, other bits are set to 0
  | Bits16 Int Word16
  | Bits32 Int Word32
  | Bits64 Int Word64
          deriving (Eq, Show)
  -- BVal BTree -- Constructor Tree

-- 5 -> BFork (BFork BVal BVal) (BFork BVal (BFork BVal BVal))
-- data BTree = BFork BTree BTree | BCon [BTree] deriving (Show,Eq)
--x = bitSplit [True,True,False,True] == [Bits8 4 208]
-- xx = bitSplit8 [True,True,False,True,False,False,False,True,True] == [Bits8 8 209,Bits8 1 128]
-- mapPM :: ([Bit] -> b) -> PatternMatcher -> (MapTypeMatcher,[Match AbsRef b])
mapPM f = second ((f <$>) <$>)

bitSplit8 :: [Bit] -> [BitMask]
bitSplit8 [] = []
bitSplit8 bs =
  let (bs', bs'') = splitAt 8 bs
  in  Bits8 (length bs') (asLSBits 8 bs') : bitSplit8 bs''

bitSplit []                   = []
bitSplit bs | length bs <= 8  = [Bits8 (length bs) (asLSBits 8 bs)]
bitSplit bs | length bs <= 16 = [Bits16 (length bs) (asLSBits 16 bs)]
bitSplit bs | length bs <= 32 = [Bits32 (length bs) (asLSBits 32 bs)]
bitSplit bs | length bs <= 64 = [Bits64 (length bs) (asLSBits 64 bs)]
bitSplit bs =
  let (bs', bs'') = splitAt 64 bs
  in  Bits64 64 (asLSBits 64 bs') : bitSplit bs''

listSplit :: Int -> [a] -> [[a]]
listSplit maxN bs
  | length bs <= maxN = [bs]
  | otherwise         = let (h, t) = splitAt maxN bs in h : listSplit maxN t

-- LSB
asLSBits :: Num c => Int -> [Bit] -> c
asLSBits n bs | n >= length bs = asNum bs

-- MSB
asMSBits :: Num c => Int -> [Bit] -> c
asMSBits n bs | n >= length bs = asNum (bs ++ replicate (n - length bs) V0)

littleEndian16 w = (w .&. 255) * 256 + shiftR w 8

-- Little endian
-- asMSBitsLE n bs | n >= length bs && n `mod` 8 == 0 = asNum (bs ++ replicate (n - length bs) False)
y = map (andMask 8) [0 .. 8]

-- m = asMSBits 16 [True,False,True]
-- n=1  -> 100000..
-- n=2  -> 11000..
andMask :: Num c => Int -> Int -> c
andMask tot n | tot >= n  = asNum (replicate n V1 ++ replicate (tot - n) V0)
              | otherwise = error $ unwords ["andMask", show tot, show n]

-- asNum bs = sum . map (\(n,b) -> if b then 2^n else 0) . zip [0..] . reverse $ bs
asNum :: Num c => [Bit] -> c
asNum =
  sum . map (\(n, b) -> if b == V1 then 2 ^ n else 0) . zip [0 ..] . reverse
  -- matchTypes :: [Match t] -> [Type t]
-- matchTypes = catMaybes . map matchType
-- matchType (MatchType t) = Just t
-- matchType _ = Nothing
-- From ZM.Dynamic
-- dd = matchTree (absTypeModel (Proxy::Proxy Word8)) --  [Bit])) -- [Bit])
