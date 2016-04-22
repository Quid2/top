{-# LANGUAGE DeriveGeneric ,NoMonomorphismRestriction #-}
import Network.Quid2

-- |Send a sensor reading every few minutes
main = runClientForever def ByType loop
     where
       loop conn = do
        reading <- readSensor2
        output conn reading
        threadDelay (minutes 3)
        loop conn

-- Fake sensor reading operations
readSensor :: IO MySensor
readSensor = return $ MySensor 15

readSensor2 :: IO (SensorReading Measure GoogleMapsLocation)
readSensor2 = return $ SensorReading (TemperatureInCelsius 15) (GoogleMapsLocation "Via+Francesco+Bocchi,+22,+50126+Firenze")

mainPrinter = runClientForever def (ByType :: ByType MySensor) loop
     where
       loop conn = input conn >>= print >> loop conn

-- Data model
data MySensor = MySensor Int -- These are Celsius by the way!
      deriving (Eq, Ord, Read, Show, Generic)

instance Flat MySensor
instance Model MySensor

data SensorReading unit place = SensorReading unit place deriving (Eq, Ord, Read, Show, Generic)

-- A location that Google Maps can make sense of
-- for example an address such as: GoogleMapsLocation "Via+Francesco+Bocchi,+22,+50126+Firenze"
data GoogleMapsLocation = GoogleMapsLocation String
       deriving (Eq, Ord, Read, Show, Generic)

data Measure = TemperatureInCelsius Int
             | Humidity Percentage
             deriving (Eq, Ord, Read, Show, Generic)

-- This means just what it seems to mean
-- data TemperatureInCelsius = TemperatureInCelsius Int deriving (Eq, Ord, Read, Show, Generic)

-- data Humidity = Humidity Percentage deriving (Eq, Ord, Read, Show, Generic)

-- |A 0..100 value
data Percentage = Percentage Int deriving (Eq, Ord, Read, Show, Generic)

instance Flat Measure
instance Model Measure

instance (Flat a,Flat b) => Flat (SensorReading a b)
instance (Model a,Model b) => Model (SensorReading a b)

instance Flat Percentage
instance Model Percentage

instance Flat GoogleMapsLocation
instance Model GoogleMapsLocation


