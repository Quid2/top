An Haskell API for the quid2-net content-addressable transport protocol.

The API is compatible with both [ghc](https://www.haskell.org/ghc/) (tested with 7.10.3) and [ghcjs](https://github.com/ghcjs/ghcjs) so it can be used to develop both stand alone and WWW applications.

### quid2-net

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

