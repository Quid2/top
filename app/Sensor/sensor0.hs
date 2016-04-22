-- Broadcast a temperature reading every few minutes

-- Import the Quid2 API
import Network.Quid2

{-
runClientForever opens a connection to a quid2 channel and keeps it alive even across transient network failures.

The parameters are:
def:
The default quid2.net network configuration, no need to change this.

ByType:
The kind of routing logic that we want to use on this connection.
'ByType' indicates that we want to receive all messages of a given type.
We do not need to indicate explicitly the type, as it is implicitly specified by the type of 'loop'.

loop:
The application code, it is passed the connection as soon as it is opened.
It uses `output` to send out the sensor reading.
-}
main = runClientForever def ByType loop
     where
       -- |The sensor reading loop
       loop conn = do
        -- Read the sensor value
        reading <- readSensor
        -- Send the value on the 'Int' channel
        output conn reading

        -- Wait and repeat
        threadDelay (minutes 3)
        loop conn

-- |Fake sensor reading operation
readSensor :: IO Int
readSensor = return 15
