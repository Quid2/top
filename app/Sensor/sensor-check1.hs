{-# LANGUAGE ScopedTypeVariables #-}
import Network.Quid2

import Sensor.Model1

-- |Collect sensor data and give warnings if needed
main = runClientForever def ByType loop
     where
       loop conn = do
         Just (MySensor temperature) <- input conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn

