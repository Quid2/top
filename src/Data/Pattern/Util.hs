module Data.Pattern.Util where

import ZM
import Data.Word
import Data.Int
import Data.Either

-- chkErrors :: [Either String b] -> [b]
-- chkErrors = either error id . collectErrors

collectErrors :: [Either String b] -> Either String [b]
collectErrors r = if null (lefts r)
                  then Right $ rights r
                  else Left $ unlines $ lefts r

stringType :: Type AbsRef
stringType = absType (Proxy::Proxy String)

charType :: Type AbsRef
charType = absType (Proxy::Proxy Char)

word8Type :: Type AbsRef
word8Type = absType (Proxy::Proxy Word8)

word16Type :: Type AbsRef
word16Type = absType (Proxy::Proxy Word16)

word32Type :: Type AbsRef
word32Type = absType (Proxy::Proxy Word32)

word64Type :: Type AbsRef
word64Type = absType (Proxy::Proxy Word64)

wordType :: Type AbsRef
wordType = absType (Proxy::Proxy Word)

int8Type :: Type AbsRef
int8Type = absType (Proxy::Proxy Int8)

int16Type :: Type AbsRef
int16Type = absType (Proxy::Proxy Int16)

int32Type :: Type AbsRef
int32Type = absType (Proxy::Proxy Int32)

int64Type :: Type AbsRef
int64Type = absType (Proxy::Proxy Int64)

intType :: Type AbsRef
intType = absType (Proxy::Proxy Int)

integerType :: Type AbsRef
integerType = absType (Proxy::Proxy Integer)

floatType :: Type AbsRef
floatType = absType (Proxy::Proxy Float)

doubleType :: Type AbsRef
doubleType = absType (Proxy::Proxy Double)
