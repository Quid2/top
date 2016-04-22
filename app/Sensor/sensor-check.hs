{-# LANGUAGE DeriveGeneric ,NoMonomorphismRestriction #-}
import Network.Quid2

-- |Send a sensor reading every few minutes
main = runClientForever def ByType loop
     where
       loop conn = do
        reading <- readSensor
        output conn reading
        threadDelay (minutes 3)
        loop conn

-- Fake sensor reading operation
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

