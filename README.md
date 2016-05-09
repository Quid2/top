Imagine visiting your favourite open shelf library or bookshop and discovering that overnight some ill-advised employee has reordered the whole collection by publishing house. All books from Oxford University Press or Penguin are now neatly grouped together.

That would look nice and orderly for sure, with all those similarly designed book spines standing side by side.

But obviously, this semblance of order would be nothing but a travesty as it would be close to impossible to locate what you are really after, and that's usually not "some book from Penguin" but rather a book about Thai cooking or the latest novel by Mo Yan.

Wake up, it was just a nightmare. Nobody would be so stupid to order human knowledge in such a nonsensical way, would they?

Except, now that you make me think about it, this is exactly how information is ordered by default in the ultimate library, the Internet. On the World Wide Web, for example, information is accessed by Web site, and a Web site is nothing but a collection of information made available from some publisher. And that, unsurprisingly, causes a bit of trouble when you are actually looking for some specific type of information.

Someone had to fix the problem, patiently reorganising the Internet library by type and subject, and in fact [someone](http://google.com) did and, as reward for their efforts, even managed to make a [little dough](http://finance.yahoo.com/q?s=GOOG) out of it. Who said that being a good librarian doesn't pay the bills?

The root of the problem is the way information is accessed and transferred on the Internet. The communication protocol at the heart of the Web, the [HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol),as in fact those used by most widely used Internet systems, is a point-to-point protocol.

Point to point simply means that information flows from A to B where A and B are publishers and/or consumers of information. It is the simplest but not always the more convenient form of communication as it's often the case that A does not in fact want to communicate with B at all, but would rather talk about *something* or exchange some specific *type* of information.

Say for example that you have some excellent jokes that you would be willing to share with the world. You don't want to call Joe and then Jake and then Marian to make them laugh. You want to make people, not a specific person, laugh. Now say that you could simply define what a joke is and then start sending them out and that anyone interested could tune in and have a good time reading them and sending out their own.

We might define a joke, in a very simple way, as:

```
data Joke = Joke String
````

that basically means: 'a joke is just a string marked as being a joke'.

And then we could simply start sending out jokes by writing a little program that says:

```
jokesChannel <- openChannel Joke 

send jokesChannel (Joke "Notice on an Italian bus: don’t talk to the driver, he needs his hands.")
```
Funny eh :-)? 

Well maybe you don't think so, but that's not a problem, you are free to send out your own jokes.

Because what we have just done, by the simple act of defining some type of information and sending out an item of that type, is to create a big fat global channel where jokes of all kinds can now flow. 

And that's precisely how top, the type oriented protocol works.


### Top (Type Oriented Protocol)

Top is a minimalist content-oriented transport protocol.

In Top, data does not flow from A to B but rather all data of the same type flows on a single, globally unique, channel.

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

Top does not provide any other service beyond full-duplex typed communication, any other service (e.g. identification or encryption) has to be provided by the clients themselves but that can be done easily and independently by simply creating data types that stands for the additional functionality required.

#### Current Implementation

Though Top is eventually meant to develop into an universal, vendor-free tranport protocol, its current incarnation is provided by a single central router.

Channel could in principle be implemented using different network protocols, currently they are based on [websockets](   https://en.wikipedia.org/wiki/WebSocket), so they are full-duplex, HTTP compatible, TCP connections.

#### APIs

This repo provides an Haskell API for Top.

The API is compatible with both [ghc](https://www.haskell.org/ghc/) (tested with 7.10.3) and [ghcjs](https://github.com/ghcjs/ghcjs) so it can be used to develop both stand alone and WWW applications.

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

