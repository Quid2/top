{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveGeneric ,NoMonomorphismRestriction ,DeriveAnyClass #-} 

module Data.Time.Util(timeDateTime,stdTimeF,formatT,timeF,Time(..),toTime,fromTime,UTCTime,getCurrentTime,currentTime) where

import           Data.Time.Clock
import           Data.Time.Format
import Data.Time.Calendar
import Data.Word
import GHC.Generics
import Data.Typed
import Control.DeepSeq

timeDateTime :: IO String
timeDateTime = timeF stdTimeF
stdTimeF = "%F %H:%M.%S"

americanTimeF = "%b %d %Y %H:%M:%S"

formatT :: FormatTime t => String -> t -> String
formatT = formatTime defaultTimeLocale

timeF :: String -> IO String
timeF format = fmap (formatT format) getCurrentTime

-- deriving instance Show UTCTime

currentTime :: IO Time
currentTime = toTime <$> getCurrentTime

data Time = Time {
   -- |Modified Julian Day, standard count of days, with zero being the day 1858-11-17
   utcDay::Integer
   -- | Seconds from midnight, 0 <= t < 86401s (because of leap-seconds)
   ,utcSecs::Word32
   } deriving (Eq,Ord,Show,Generic,Flat,Model,NFData)

toTime :: UTCTime -> Time
toTime utcTime = Time (toModifiedJulianDay . utctDay $ utcTime) (toEnum . fromEnum . (/1000000000000) . utctDayTime $ utcTime)

fromTime t = UTCTime (ModifiedJulianDay $ utcDay t) (secondsToDiffTime . fromIntegral . utcSecs  $ t)
