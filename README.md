An Haskell API for the quid2-net content-addressable transport protocol.

The API is compatible with both [ghc](https://www.haskell.org/ghc/) (tested with 7.10.3) and [ghcjs](https://github.com/ghcjs/ghcjs) so it can be used to develop both stand alone and WWW applications.

### Top (type oriented protocol), a minimalist content-oriented transport protocol

Most widely used Internet transport protocols are point-to-point, think [TCP](https://en.wikipedia.org/wiki/Transmission_Control_Protocol) or [HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol). 

top is, on the contrary, a content-oriented protocol.

Data does not flow from A to B but rather all data of the same type flows on a single, globally unique, channel.

Briefly:
* For every data type there is a corresponding channel
* A channels transfer only data values of its associate type
* Anyone can send and receive data values

You can [see it in action](http://quid2.org/app/ui). 

Under the *Channels* tab are listed the currently open channels, every channel has a type and you can see its full definition by clicking on *Definition*.

Definitions are just plain algebraic data types (with a couple of restrictions: data types definitions cannot be mutually recursive and variables can appear only in kind * positions). 

For example, you should see a *Message* channel that is used to implement a simple chat system. Click on *Show Values* to inspect the value being transferred and then use the [chat user interface](http://quid2.org/app/chat) to login and send a couple of messages and see them appear on the channel.

Under the *Types* tab, is the list of types known to the system.

#### Minimalist and Evolvable

quid2-net does not provide any other service beyond full-duplex typed communication, any other service (e.g. identification or encryption) has to be provided by the clients themselves but that can be done easily and independently by simply creating data types that stands for the additional functionality required.

#### Current Implementation

Though quid2-net is eventually meant to develop into an universal, vendor-free tranport protocol, its current incarnation is provided by a single central router.

Channel could in principle be implemented using different network protocols, currently they are based on [websockets](   https://en.wikipedia.org/wiki/WebSocket), so they are full-duplex, HTTP compatible, TCP connections.

#### APIs

Currently the quid2-net API is available only for the [Haskell](http://www.haskell.org) language, see  [quid2-net](https://github.com/tittoassini/quid2-net).

APIs for other programming languages are planned and help would be greatly appreciated.

### How Do I Use It?

Install Quid2 as specified below and then look at [quid2-net-apps](https://github.com/tittoassini/quid2-net-apps) and [quid2-net-apps-ghcjs](https://github.com/tittoassini/quid2-net-apps-ghcjs) for examples of how to develop stand-alone and www applications.


quid2-net is a *simple*, *accurate* and *free* messaging service.

#### Simple

Using quid2-net can be as simple as:

```haskell
{-# LANGUAGE DeriveGeneric #-}
import Network.Quid2

-- |Send a message and then print out all messages received
main = runClient def ByType $ \conn -> do
  output conn Message {fromUser="robin",content=TextMessage "Hello!"}
  loop conn
    where loop conn = input conn >>= print >> loop conn

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
Sounds interesting? Check some [stand-alone](https://github.com/tittoassini/quid2-net-apps) and [WWW ](https://github.com/tittoassini/quid2-net-apps-ghcjs) applications.

### Downtime
quid2-net might be down for upgrades every Monday between 7 and 8 am (UTC+1 [DST](https://en.wikipedia.org/wiki/Daylight_saving_time)).

