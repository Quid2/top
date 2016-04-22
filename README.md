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

Sounds interesting? Check the [tutorial](tutorial.md).

### Installation

quid2-net depends on other unreleased packages that are part of the quid2 suite, so that's what we need to install:

`git clone --recursive https://github.com/tittoassini/quid2.git;cd quid2;stack build`

The first time, the installation can take a few minutes.

To verify that all works, start up the `quid2-chat` program:

```
stack exec quid2-chat

Enter your name:
titto

Help:
To send a message: just enter it and press return.
To exit: Ctrl-D.

Current Subject: (quid2-net)

Hello!
```

That's it!

##### To update to the latest release:

`cd quid2`

`git pull;git submodule update --remote;stack build`

### Downtime
quid2-net might be down for upgrades every Monday between 7 and 8 am (UTC+1 [DST](https://en.wikipedia.org/wiki/Daylight_saving_time)).

### Support
Problems? Questions? Open an **Issue** on this repository or write directly to *titto* at: *tittoassini@gmail.com*
