The communication model of quid2-net is extremely simple:
* For every serialisable data type there is a corresponding channel
* A channels transfer only data values of its associate type
* Anyone can send and receive data values

It's essentially a minimalist content addressable network.

quid2-net does not provide any other service beyond full-duplex typed communication, any other service (e.g. identification or encryption) has to be provided by the clients themselves.

Channel could in principle be implemented using different network protocols, currently they are based on [websockets](   https://en.wikipedia.org/wiki/WebSocket), so they are full-duplex, HTTP compatible, TCP connections.

Currently the quid2-net API is available only for the [Haskell](http://www.haskell.org) language.

APIs for other programming languages are planned (JavaScript is next).

Using the Haskell API is straightforward:

* define a data model, that's just one or more serialisable Haskell data types 
* automatically derive instances of the *Flat* (serialisation) and *Model* (introspection) classes
* connect to one or more typed channels and send/receive values and get in communication with any other client using the same types

#### Example: collecting data from distributed sensors

Suppose that you want to keep an eye on your house while at work, and in particular check for possible fires.

A `sensor` program that reads the temperature from a sensor and broadcast it using quid2.net would be:

```haskell
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
It uses `send` and `receive` to communicate.
-}
main = runClientForever def ByType loop
     where
       -- |The sensor reading loop
       loop conn = do
        -- Read the sensor value
        reading <- readSensor
        -- Send the value on the 'Int' channel
        send conn reading

        -- Wait and repeat
        threadDelay (minutes 3)
        loop conn

-- |Fake sensor reading operation
readSensor :: IO Int
readSensor = return 15
```

We will also need a `sensor-check` program to collect the data and print it out: 

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Network.Quid2

-- |Collect sensor data and give warnings if needed
main = runClientForever def (ByType::ByType Int) loop
     where
       loop conn = do
         temperature :: Int <- receive conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn
```
It could not be easier than this.

There is just a little issue, as we mentioned, in quid2.net there is a channel for every possible serialisable type.

Our data will therefore travel over the 'Int' channel.

Obviously other users might use this channel for their own purposes and then it would be rather hard to distinguish our Ints from those of anyone else and whose meaning has nothing to do at all with temperature readings.

Time to develop a slightly more sophisticated data model.

As it will need to be shared between the `sensor` and the `sensor-check` program, we will put the data model in a separate module:

```haskell
{-# Language DeriveGeneric #-}
module Sensor.Model1 where

import Data.Typed

-- A rather asinine data model
data MySensor = MySensor Int -- These are Celsius by the way!
              deriving (Eq, Ord, Read, Show, Generic)

instance Flat MySensor
instance Model MySensor
```

Our data type needs to be an instance of the *Flat* (serialisation) and *Model* (introspection) classes, the instances are automatically derived, provided that the type derives `Generic`.

The `sensor` program has barely changed:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Network.Quid2

import Sensor.Model1

main = runClientForever def ByType loop
     where
       loop conn = do
        reading <- readSensor
        send conn reading
        threadDelay (minutes 3)
        loop conn

readSensor :: IO MySensor
readSensor = return $ MySensor 15
```

and similarly `sensor-check`:

```haskell
{-# LANGUAGE ScopedTypeVariables #-}
import Network.Quid2

import Sensor.Model1

-- |Collect sensor data and give warnings if needed
main = runClientForever def ByType loop
     where
       loop conn = do
         MySensor temperature <- receive conn
         print $ show temperature ++ " Celsius"
         when (temperature > 50) $ print "ALARM, HOUSE ON FIRE!!!!"
         loop conn
```

So now we are transferring our data on the `MySensor` channel, that makes a bit more sense, at least to us. 

But what if we have more than one kind of sensors or if our sensors are in different locations, maybe a temperature sensor at home and a humidity sensor in the allotment garden? 

More importantly, what if our friends also want to access their sensors? 

There might be some value in a versatile and open distributed sensor network but to support it we need a much richer and shareable model that others might be willing to adopt.

And here we come to the main point about quid2.net: to facilitate large scale data exchange by progressively converging on a shared lexicon of data types.

Before inventing your own types you should check the [list of currently known types](http://quid2.net:8000).

The more you reuse existing concepts, the greater the value of your data and programs.

#### Establish Provenance and Preserve Data Integrity

It's all fine and dandy till you receive a fire alarm and run to your house just to discover that your friend Bob, a notorious prankster, has sent you fake readings pretending to be your faithful temperature sensor.

Remember, quid2-net channels are public, anyone can send anything.

A simple way to establish provenance, is to sign your values.

We start by defining a type for signed values:

```haskell
-- |A signed value
data Signed value signature = Signed value signature
```

Then one or more types to represent specific signature algorithms, for example [Ed25519](http://ed25519.cr.yp.to/):

```haskell
-- |An Ed255619 signature
data Ed255619 = Ed255619 [Word8] deriving (Eq, Ord, Read, Show, Generic)
```
By embedding our values in `Signed Ed255619`, we can avoid being pranked again. 

For an example, see [`signed`](app/signed.hs).

As a bonus, this will also guarantee the messages' integrity, if anyone had tampered with them in any way, the signature won't match.

#### And so on ...

A similar procedure can be followed to add encryption, data compression or any other required feature in a flexible, autonomous and incremental way.

#### Examples

Have a look at some examples of clients/bots:
* [`hello`](app/hello.hs)
   * Simplest possible self-contained agent
* [`chat-history`](app/Chat/chat-history.hs)
   * A simple bot that will store all messages conforming to a simple chat data model and will send them back on request 
* [`chat`](app/Chat/chat.hs)
   * Basic end-user client (multiple channels, usage of Pipes). 
* [`signed`](app/signed.hs)
   * Using cryptographic signatures to establish provenance and preserve data integrity

To run the corresponding executable from command line:
`stack exec quid2-<example_name>`

For example: `stack exec quid2-hello`.

#### API Documentation

Start with:
* [Network.Quid2.Run](src/Network/Quid2/Run.hs)
* [Network.Quid2.Pipes](src/Network/Quid2/Pipes.hs)
* [Network.Quid2.Types](src/Network/Quid2/Types.hs)
