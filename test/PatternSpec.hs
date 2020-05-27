{-# LANGUAGE BangPatterns              #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TemplateHaskell           #-}
{-# LANGUAGE TupleSections             #-}
{-# LANGUAGE UndecidableInstances      #-}
module Main where

import           Control.Applicative
import           Data.Bifunctor
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy          as L
import           Data.Digest.Keccak
import           Data.Foldable
import           Data.Int
import           Data.List
import qualified Data.Map                      as M
import           Data.Maybe
import           Data.Model              hiding ( Con )
import           Data.Pattern
import           Data.Pattern.Matcher
import qualified Data.Text                     as T
import qualified Data.Text
import           ZM                      hiding ( Con
                                                , Cons
                                                , Val
                                                , Var
                                                )
import           Data.Word
import           Debug.Trace
import           System.Exit                    ( exitFailure )
import           Test.Data               hiding ( V1 )
import           Test.Data.Flat          hiding ( V1 )
import           Test.Data.Model
import           Test.Tasty
import qualified Test.Tasty                    as T
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck         as QC
import           Control.Exception
import           System.IO.Unsafe


-- import Data.Pattern.X64(match,matcher)
import           Data.Pattern.Matcher.Haskell   ( matchPM
                                                , match
                                                , matcher
                                                )

-- RUN AS: stack test --file-watch :pattern-test
t = main

-- FIX
-- pstr2 = patternQ

  --print (B.unpack $ ser (Cons False (Cons True Nil)))
  -- let tst proxy hpat a =
  --       let
  --         ByPattern pat = byPattern hpat
  --         at = absTypeModel proxy
  --         pp = patternMatcher at pat
  --         chk = matchPM pp
  --         bs = ser a
  --       in bench (unwords["match",show pat,"vs",take 60 $ show a]) $ nf chk bs


main = do
  -- let m = matchByte 11
  -- m1 <- matchG m (B.pack [44,55,88])
  -- print m1
  -- m2 <- matchG m (B.pack [11,55,88])
  -- print m2

  -- let proxy = Proxy :: Proxy Bool
  -- pat <- patternQ [p|False|]
  -- -- let vs = [[],[129],[1],[0],[1,1],[0,b0,0]]
  -- -- let proxy = Proxy :: Proxy [Bool]
  -- -- pat <- patternQ [p|Cons False (Cons _ Nil)|]
  -- let vs = [[128+32+1]]
  -- let at = absTypeModel proxy
  -- --print $ pattern2Match at pat
  -- --print $ patternMatcher at pat

  -- let pp = patternMatcher at pat
  -- -- print $ X.patternMatcher_ pp

  -- let mm = matchPM pp
  -- let mr = map (mm . B.pack) vs
  -- print "match results"
  -- print mr

  -- p1 <- patternQ [p|_|]
  -- p2 <- patternQ [p|'c'|]
  -- p3 <- patternQ [p|11|]
  -- p4 <- patternQ [p|111111|]
  -- p5 <- patternQ [p|"abcdefghilmnopqrstuvz"|]
  -- p6 <- patternQ [p|True|]
  -- p7 <- patternQ [p|Cons False (Cons True Nil)|]
  -- p8 <- patternQ [p|Cons _ (Cons _ Nil)|]
  ps <- pats
  T.defaultMain (tests (matchPM, ps))-- [p1,p2,p3,p4,p5,p6,p7,p8]))

-- tests :: TestTree
tests pats = testGroup "Tests"
                       [--properties
                        patternTests pats
                               --,matchTests
                                         ]

properties = testGroup "Router Properties" []

patternTests (match, [wild, char, n, n2, str0, str, str1, str2, true, list, listWild, listWildS])
  = testGroup
    "Pattern Tests"
    [ testPatQ wild (PName PWild)
    , testPatQ char (PName $ PChar 'c')
    , testPatQ n    (PName (PInt 11))
    , testPatQ n2   (PName (PInt 111111))
    , testPatQ str0 (PName (PString "abc"))
    , testPatQ str1 (PCon "Cons" [PName (PChar 'a'), PName PWild])
    , testPatQ true (PCon "True" [])
    , testPatQ
      list
      (PCon "Cons"
            [PCon "False" [], PCon "Cons" [PCon "True" [], PCon "Nil" []]]
      )
    , testPatQ
      listWild
      (PCon "Cons" [PName PWild, PCon "Cons" [PName PWild, PCon "Nil" []]])
    , testPatQ
      listWildS
      (PCon "Cons" [PName PWild, PCon "Cons" [PCon "True" [], PCon "Nil" []]])
    , testByPat (Proxy :: Proxy Char) wild
      $ Right [MatchAny (TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)))] -- Left ""-- (Right [MatchAny (TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29)))])
    , testByPat (Proxy :: Proxy [Bool])
                list
                (Right [MatchValue [V1, V0, V1, V1, V0]])
    , testByPat (Proxy :: Proxy [Bool]) listWild $ Right
      [ MatchValue [V1]
      , MatchAny (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
      , MatchValue [V1]
      , MatchAny (TypeCon (AbsRef (SHAKE128_48 48 111 25 129 180 28)))
      , MatchValue [V0]
      ]
  -- ,testByPat (Proxy :: Proxy ([Bool])) listWild (Right [
  --                                                MatchValue [True]
  --                                               ,MatchAny (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
  --                                               ,MatchValue [True]
  --                                               ,MatchAny (TypeCon (AbsRef (SHA3_256_6 129 212 40 48 111 29)))
  --                                               ,MatchValue [False]])
    , testByPat (Proxy :: Proxy Char) true
      $ Left
          "Constructor 'True' not present in TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69))\n" -- Left "Constructor 'True' not present in TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29))\n"
    , testByPat (Proxy :: Proxy Char) n
      $ Left
          "Type mismatch: expected TypeCon (AbsRef (SHAKE128_48 6 109 181 42 241 69)) type, found 11\n"-- (Left "Type mismatch: expected TypeCon (AbsRef (SHA3_256_6 7 117 93 14 24 29)) type, found 11\n")
    , testPatBin (Proxy :: Proxy Bool) true (B.pack [129])       True
    , testPatBin (Proxy :: Proxy Bool) true (B.pack [1])         False
    , testPatBin (Proxy :: Proxy Bool) true (B.pack [128])       False
    , testPatBin (Proxy :: Proxy Bool) true (B.pack [129, 1])    False
    , testPatBin (Proxy :: Proxy Bool) true (B.pack [129, 1, 1]) False
    , testPat list      (Cons False (Cons True Nil)) True
    , testPat listWild  (Cons False (Cons True Nil)) True
    , testPat listWild  (Cons True (Cons False Nil)) True
    , testPat listWild  (Cons False Nil)             False
    , testPat listWildS (Cons False (Cons True Nil)) True
    , testPat wild      longList                     True
    , testPat char      'a'                          False
    , testPat char      'c'                          True
    , testPat wild      'c'                          True
    , testPat str ("abcdefghilmnopqrstuvz" :: String) True
    , testPat str1      ("abcdefz" :: String)        True
    , testPat str2 ("abcdefghilmnopqrstuvz" :: String) True
    , testPat str2 ("abcdZfghilmnopqrstuvz" :: String) False
    , testPat n         (11 :: Word)                 True
    , testPat n         (11 :: Word32)               True
    , testPat n         (11 :: Int)                  True
    , testPat n         (11 :: Int32)                True
    , testPat n         (11 :: Integer)              True
    , testPat n         (10 :: Int)                  False
    , testPat n2        (111111 :: Integer)          True
    ]

-- Test TH parsing
testPatQ hpat pat = testCase (unwords ["Pattern", show pat]) $ hpat @?= pat

-- Test conversion of TH patterns to ByPattern-expected format
testByPat
  :: forall a
   . (Model a)
  => Proxy a
  -> Pat PRef
  -> Either String Pattern
  -> TestTree
testByPat _ hpat epat =
  testCase (unwords ["testByPat", show hpat])
    $   (   (\(ByPattern pat) -> pat)
        <$> (byPattern_ hpat :: Either String (ByPattern a))
        )
    @?= epat

-- Test full pattern matching pipeline: haskell (TH) pattern matching values
testPat
  :: forall a . (Model a, Flat a, Show a) => Pat PRef -> a -> Bool -> TestTree
testPat hpat val exp =
  let ByPattern pat = byPattern hpat :: ByPattern a
      checker       = chk (Proxy :: Proxy a) pat
      bs            = ser val
  in  testCase (unwords ["testPat", show val]) $ checker bs @?= exp

testPatBin
  :: forall a
   . (Model a)
  => Proxy a
  -> Pat PRef
  -> B.ByteString
  -> Bool
  -> TestTree
testPatBin proxy hpat bs exp =
  let ByPattern pat = byPattern hpat :: ByPattern a
      checker       = chk proxy pat
  in  testCase (unwords ["testPatBin", show pat, show $ B.unpack bs])
        $   checker bs
        @?= exp

chk proxy pat = match (matcher (patternMatcher (absTypeModel proxy) pat))

ser = flat

-- test patterns
pats = mapM
  patternQ
  [ [p|_|]
  , [p|'c'|]
  , [p|11|]
  , [p|111111|]
  , [p|"abc"|]
  , [p|"abcdefghilmnopqrstuvz"|]
  , [p|'a':_|]
  , [p|'a':_:_:_:'e':'f':'g':'h':_:_:_:_:_:_:_:'r':'s':'t':'u':'v':_|]
  , [p|True|]
  , [p|Cons False (Cons True Nil)|]
  , [p|Cons _ (Cons _ Nil)|]
  , [p|_:True:[]|]
  ]

longList = replicate 1000 False

pk = B.pack


instance Exception String

p = do
  --print $ (pat [p|False|] :: Bool)
  let p0   = $(patternE [p|0|])
  let p255 = $(patternE [p|255|])
  mapM_
    print
    [ w (byPattern $(patternE [p|False|]) :: ByPattern Bool)
    --,w (byPattern $(patternE [p|III {w8=11,w16=22}|]) :: ByPattern III)
    , w (byPattern $(patternE [p|_|]) :: ByPattern Bool)
    -- ,w (byPattern $(patternE [p|Msg "sj" _ _|]) :: ByPattern Msg)
    -- ,w (byPattern $(patternE [p|Msg _ (Subject ("Hask":_:[])) (TextMsg "hello")|]) :: ByPattern Msg)
    , w
      (byPattern $(patternE [p|(5,3.3,5.5)|]) :: ByPattern
          (Word16, Float, Double)
      )
    , w
      (byPattern $(patternE [p|(False,True,False,True)|]) :: ByPattern
          (Bool, Bool, Bool, Bool)
      )
    , w (byPattern p0 :: ByPattern Word8)
    , w (byPattern p0 :: ByPattern Word16)
    , w (byPattern p0 :: ByPattern Word32)
    , w (byPattern p0 :: ByPattern Word64)
    , w (byPattern p0 :: ByPattern Word)
    , w (byPattern p0 :: ByPattern Int8)
    , w (byPattern p0 :: ByPattern Int16)
    , w (byPattern p0 :: ByPattern Int32)
    , w (byPattern p0 :: ByPattern Int64)
    , w (byPattern p0 :: ByPattern Int)
    , w (byPattern p0 :: ByPattern Integer)
    , w (byPattern p255 :: ByPattern Word8)
    , w (byPattern p255 :: ByPattern Word16)
    , w (byPattern p255 :: ByPattern Word32)
    , w (byPattern p255 :: ByPattern Word64)
    , w (byPattern p255 :: ByPattern Word)
    , w (byPattern p255 :: ByPattern Int8)
    , w (byPattern p255 :: ByPattern Int16)
    , w (byPattern p255 :: ByPattern Int32)
    , w (byPattern p255 :: ByPattern Int64)
    , w (byPattern p255 :: ByPattern Int)
    , w (byPattern p255 :: ByPattern Integer)
    , w (byPattern p255 :: ByPattern Integer)
    ]
  where w (ByPattern p) = p



-- mainTest0 = do
--   [wild,char,n,n2,str0,str,str1,str2,true,list,listWild,listWildS] <- pats

--   let pxBool = (Proxy :: Proxy Bool)
--   let pxBools = (Proxy :: Proxy (Bool,Bool,Bool))
--   let pxWords = (Proxy :: Proxy (Bool,Integer,Integer,Bool))
--   let pxBoolList = (Proxy :: Proxy ([Bool]))
--   pfalse <- patternQ [p|False|]
--   ptrue <- patternQ [p|True|]
--   pbools <- patternQ [p|(True,False,True)|]
--   pwords <- patternQ [p|(False,11,22,True)|]
--   pstr <- patternQ [p|'a':_|]
--   -- pat <- patternQ [p|Cons False (Cons _ Nil)|]
--   --print $ pattern2Match at pat

--   --testInstructions

--   -- tst pxBoolList wild (B.unpack $ ser $ Cons False (Cons True (Cons False Nil))) True >> tst pxBoolList wild [0] False >> tst pxBoolList wild [0,1] False

--   print ""
--   -- mapM (\t -> t True) [
--   --   tst (Proxy :: Proxy Char) char 'c'
--   --   ,tst (Proxy :: Proxy String) str ("abcdefghilmnopqrstuvz"::String)
--   --   ,tst (Proxy :: Proxy [Bool]) listWild (Cons False (Cons True Nil))
--   --   ,tst (Proxy :: Proxy [Bool]) wild longList
--   --   ,tst (Proxy :: Proxy Char) wild 'c'
--   --   ,tst (Proxy :: Proxy Word8) n (11::Integer)
--   --   ,tst (Proxy :: Proxy Word8) n (11::Int8)
--   --   ,tst (Proxy :: Proxy Word16) wild (11::Word16)
--   --   ,tst (Proxy :: Proxy String) str2 ("abcdefghilmnopqrstuvz"::String)
--   --   ]
--   tst (Proxy :: Proxy [Bool]) wild (Cons False (Cons True Nil)) True
--   -- tst (Proxy :: Proxy String) pstr ("abcdefghilmnopqrstuvz"::String) True
--   -- tst (Proxy :: Proxy String) str1 ("abcdefz"::String) True
--   -- tst (Proxy :: Proxy String) str2 ("abcdefghilmnopqrstuvz"::String) True
--   --tst (Proxy :: Proxy String) str1 ("ab"::String) True
--   --tst (Proxy :: Proxy Char) wild (ser 'c') True
--   -- tst (Proxy :: Proxy String) str (ser ("abcdefghilmnopqrstuvz"::String)) True
--   -- tst (Proxy :: Proxy String) str2 (ser ("abcdefghilmnopqrstuvz"::String)) True  
--   -- tst pxBool pfalse [1] True >> tst pxBool pfalse [128+1] False >> tst pxBool ptrue [1] False >> tst pxBool ptrue [128+1] True
--   -- print (map X.fix8 [127,128,129])
--   -- tst pxBools pbools [128+1] False >> tst pxBools pbools [128+32+1] True
--   -- print $ B.unpack $ ser (44::Word16,22::Word16,33::Word16)
--   -- print $ B.unpack $ ser (44::Integer,22::Integer,33::Integer)

--   -- tst pxWords pwords (B.unpack $ ser (False,11::Integer,22::Integer,True)) True
--   -- tst pxWords pwords (B.unpack $ ser (False,11::Integer)) False
--   -- tst pxWords pwords (B.unpack $ ser (False,11::Integer,22::Integer,False)) False

--     where tst proxy pat a exp = do
--             let vs = ser a
--             let at = absTypeModel proxy
--             let pp@(tt,mm) = patternMatcher at pat
--             print mm
--             mapM_ (\(n,c) -> print n >> print c >> putStrLn "") (M.toList tt)
--             print $ (B.unpack vs,B.length vs)
--             --let [MatchBits bs] = mm in print (length bs,bs)

--             --print $ matcherCode pp
--             let r = matchPM pp vs
--             -- print r
--             print $ r == exp
