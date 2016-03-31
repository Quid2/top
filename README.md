### quid2-net

quid2-net is a *simple*, *accurate* and *free* messaging service.

#### Simple

Using quid2-net can be as [simple](app/hello.hs) as:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Network.Quid2

-- |Send a message and then print out all messages received
main = runClient def ByType $ \conn -> do
  send conn Message {fromUser="robin",content=TextMessage "Hello!"}
  loop conn
    where loop conn = receive conn >>= print >> loop conn

-- Data model for a very simple chat system
data Message = Message {
     fromUser::String
    ,content::Content
    } deriving (Eq, Ord, Read, Show, Generic)

data Content =
    TextMessage String

    | HTMLMessage String
    deriving (Eq, Ord, Read, Show, Generic)

instance Flat Message
instance Flat Content
instance Model Message
instance Model Content
```

#### Accurate

In quid2-net, all communication takes place on bi-directional *typed* channels, that's to say on channels that transfer only values of a well-defined algebraic data type.

#### Free

TERMS OF SERVICE: 
* The quid2-net service is offered by Quid2 Limited - Registered in England and Wales - Reg No: 09213600.
* There is no charge for fair usage of the quid2-net service. 
* Fair usage is defined as any usage that does not lead to a *de facto* denial of service to other users or that imposes unreasonable expense on its maintainer.
* By using quid2-net you accept that the service is offered "as is" with no express or implied warranty for availability, performance, consistency, longevity or functionality.

### Usage

The communication model of quid2-net is extremely simple:
* For every serialisable data type there is a corresponding channel.
* Anyone can send and receive data values of the appropriate type on any channel.

It's essentially a minimalist content addressable network.

quid2-net does not provide any other service beyond full-duplex typed communication, any other service (e.g. identification or encryption) has to be provided by the clients themselves.

Channel could in principle be implemented using different network protocols, currently they are based on [websockets](   https://en.wikipedia.org/wiki/WebSocket), so they are full-duplex, HTTP compatible, TCP connections.

Currently the quid2-net API is available only for the [Haskell](http://www.haskell.org) language.

APIs for other programming languages are planned (JavaScript is next).

Using the Haskell API is straightforward:

* define a data model, that's just one or more serialisable Haskell data types 
* automatically derive instances of the *Flat* (serialisation) and *Model* (introspection) classes
* connect to one or more typed channels and send/receive values and get in communication with any other client using the same types

#### Documentation and Examples

Have a look at some examples of clients/bots:
* [`hello`](app/hello.hs)
   * Simplest possible self-contained agent
* [`chat-history`](app/Chat/chat-history.hs)
   * A simple bot that will store all messages conforming to a simple chat data model and will send them back on request 
* [`chat`](app/Chat/chat.hs)
   * Basic end-user client

Read the API, in particular:
* [Network.Quid2.Run](src/Network/Quid2/Run.hs)
* [Network.Quid2.Pipes](src/Network/Quid2/Pipes.hs)  
* [Network.Quid2.Types](src/Network/Quid2/Types.hs)  
