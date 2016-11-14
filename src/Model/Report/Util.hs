 {-# LANGUAGE OverloadedStrings           #-}
module Model.Report.Util(
  --getByType,getServerState
  reportURL,printReport,byAnyReport,byTypeReport,byPatternReport
  ) where
import Data.Typed
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Model.Report
import Control.Monad
import Data.Maybe
import Network.Top.Types(Config(..),def,cfgIP,cfgPort)

reportURL cfg = concat["http://",cfgIP cfg,":",show (cfgPort cfg),"/report"]

bytes = L.unpack . unblob

byAnyReport st = let [ByAnyReport vs] = byAnyReport_ st
                 in vs

byAnyReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byAnyReportType)
   then [dec (bytes b)::ByAnyReport]
   else concatMap byAnyReport_ ss

byTypeReport st = let [ByTypeReport vs] = byTypeReport_ st
                  in vs

byTypeReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byTypeReportType)
   then [dec (bytes b)::ByTypeReport]
   else concatMap byTypeReport_ ss

byPatternReport st = let [ByPatternReport vs] = byPatternReport_ st
                     in vs

byPatternReport_ (NestedReport n (TypedBLOB t b) ss) =
   if (t == byPatternReportType)
   then [dec (bytes b)::ByPatternReport]
   else concatMap byPatternReport_ ss

printReport (NestedReport n (TypedBLOB t b) ss) = do
  let bs = bytes b
  putStrLn n
  print t
  print ("warpReportType",warpReportType)
  when (t == warpReportType) $ print (dec bs::WarpReport)
  when (t == byAnyReportType) $ print (dec bs::ByAnyReport)
  when (t == byTypeReportType) $ print (dec bs::ByTypeReport)
  when (t == byPatternReportType) $ print (dec bs::ByPatternReport)
  when (t == echoReportType) $ print (dec bs::[ClientReport])
  mapM_ printReport ss

dec bs = let Right a = unflat (L.pack bs) in a

warpReportType = absType (Proxy::Proxy WarpReport)
stringType = absType (Proxy::Proxy String)
intType = absType (Proxy::Proxy Int)
byAnyReportType = absType (Proxy::Proxy ByAnyReport)
byTypeReportType = absType (Proxy::Proxy ByTypeReport)
byPatternReportType = absType (Proxy::Proxy ByPatternReport)
echoReportType = absType (Proxy::Proxy [ClientReport])

