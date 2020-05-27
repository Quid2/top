module Data.Pattern.Matcher.Haskell
  ( matchPM
  , match
  , matcher
  ) where

import           Control.Monad
import           Data.Bits
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L
import           Data.Either
import           Flat
import           Flat.Decoder
import           Data.Model           (solve)
import           Data.Pattern.Bits
import           Data.Pattern.Matcher
import           Debug.Trace

matchPM :: PatternMatcher -> B.ByteString -> Bool -- (Either MatchError Bool)
matchPM = match . matcher

-- match :: Get Bool -> B.ByteString -> Bool
match dec bs = either (const False) (const True) $ unflatWith dec bs

-- match matcher bs = case runPartialGet matcher (L.fromStrict bs) 0 of
--                 Left _ -> False
--                 -- Left "f" -> False
--                 -- Left "not enough bytes" -> False -- Left NotEnoughData
--                 Right (True,bs,usedBits) -> (L.length bs==1 && shiftR (shiftL (L.head bs) usedBits) usedBits == 1) || (L.length bs==2 && usedBits==8 && L.head (L.tail bs) == 1)
--                 -- Right (True,bs,usedBits) -> error. unwords $ ["Unexpected return from runPartialGet",show $ L.length bs,show usedBits]
-- match_  bs = runPartialGet matchAll (L.fromStrict bs) 0
-- match_ :: PatternMatcher -> B.ByteString -> Bool -- (Either MatchError Bool)
matcher :: PatternMatcher -> Get ()
matcher = match__ . mapPM bitSplit

match__ (tt, pat) =
  mapM_ matchPattern pat
  where
    matchPattern (MatchAny t)      = matchType t
    matchPattern (MatchValue bits) = mapM_ matchBits bits
    matchBits (Bits8 n v)  = tst n v dBEBits8
    matchBits (Bits16 n v) = tst n v dBEBits16
    matchBits (Bits32 n v) = tst n v dBEBits32
    matchBits (Bits64 n v) = tst n v dBEBits64
      --   r <- xor v <$> dBEBits64 n
      --   return (r==0)  -- when (r/=0) $ fail "f"
    matchType t = matchTree (solve t tt)
    matchTree (BFork l r) = do
      b <- dBool
      if b
        then matchTree r
        else matchTree l
    matchTree (BCon ts) = mapM_ matchType ts
    matchTree (Skip n) = dropBits n
    tst n v d = do
      decoded <- d n
      let r = xor v decoded
      traceM $
        unwords ["compare", show n, "bits of", show v, "with", show decoded]
      when (r /= 0) $ fail "matcher failed"
-- import Control.Monad.State.Strict
-- import Control.Monad.Catch
-- -- type M = StateT State (Either String) ()
-- data MatcherState = Matcher State {bs::B.ByteString
--                                   ,validBits::Int -- valid bits in first byte
--                                   }
-- t = runStateT (MatcherState (B.pack [129]) 0) $ getBits8 3
-- getBits8 n | n > 0 = do
--   vb <- gets validBits
--   when (vb < n) $ throwM NotEnoughData
--   shiftL (8-vb) . B.head <$> gets bs
