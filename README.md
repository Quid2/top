
[![Build Status](https://travis-ci.org/Quid2/top.svg?branch=master)](https://travis-ci.org/Quid2/top)
[![Hackage version](https://img.shields.io/hackage/v/top.svg)](http://hackage.haskell.org/package/top)
[![Stackage Nightly](http://stackage.org/package/top/badge/nightly)](http://stackage.org/nightly/package/top)
[![Stackage LTS](http://stackage.org/package/top/badge/lts)](http://stackage.org/lts/package/top)

Haskell API for [Top (Type Oriented Protocol)](http://quid2.org/docs/Top.pdf).

### Why Bother?

Imagine visiting your favourite open shelf library or bookshop and discovering that overnight some ill-advised employee has reordered the whole collection by publishing house. All books from Oxford University Press or Penguin are now neatly grouped together.

That would look nice and orderly for sure, with all those similarly designed book spines standing side by side.

But obviously, this semblance of order would be nothing but a travesty as it would be close to impossible to locate what you are really after, and that's usually not "some book from Penguin" but rather a book about Thai cooking or the latest novel by Mo Yan.

Wake up, it was just a nightmare. Nobody would be so stupid to order human knowledge in such a nonsensical way, would they?

Except, now that you make me think about it, this is exactly how information is ordered by default in the ultimate library, the Internet. On the World Wide Web, for example, information is accessed by website, and a website is nothing but a collection of information made available from some publisher. And that, unsurprisingly, causes a bit of trouble when you are actually looking for some specific type of information.

Someone had to fix the problem, patiently reorganising the Internet library by type and subject, and in fact [someone](http://google.com) did and, as reward for their efforts, even managed to make a [little dough](http://finance.yahoo.com/q?s=GOOG) out of it. Who said that being a good librarian doesn't pay the bills?


The root of the problem is the way information is accessed and transferred on the Internet. The communication protocol at the heart of the Web, the [HTTP](https://en.wikipedia.org/wiki/Hypertext_Transfer_Protocol), as in fact those of most widely used Internet systems, is a point-to-point protocol.

Point-to-point simply means that information flows from A to B where A and B are publishers and/or consumers of information. It is the simplest but not always the most convenient form of communication as it's often the case that A does not in fact want to communicate with B at all, but would rather talk about *something* or exchange some specific *type* of information.

Say for example that you have some excellent jokes that you would be willing to share with the world. You don't want to call Joe and then Jake and then Marian to make them laugh. You want to make people, not a specific person, laugh. Now say that you could simply define what a joke is and then start sending them out and that anyone interested could tune in and have a good time reading them and sending out their own.

We might define a joke, in a very simple way, as:

```haskell
Joke ≡ Joke Text
````
that basically means: 'a joke is just a text marked as being a joke'.

And then we could simply start sending out jokes by writing a little program that says something like:

```haskell
jokesChannel <- openChannelOfType Joke

send jokesChannel (Joke "Notice on an Italian bus: don’t talk to the driver, he needs his hands.")
```
Funny eh :-)?

Maybe not, but that's not a problem, you are free to send out your own jokes.

Because what we have just done, by the simple act of defining some type of information and sending out an item of that type, is to create a big fat global channel where jokes can now flow.

And that's precisely how Top, the type oriented protocol, works.

### Top (Type Oriented Protocol)

Top is a minimalist content-oriented transport protocol ([spec](http://quid2.org/docs/Top.pdf)).

In Top, all communication takes place on bi-directional *typed* channels, that's to say on channels that transfer only values of a well-defined algebraic data type.

Data does not flow from A to B but rather all data of the same type flows on a single, globally unique, channel and anyone can define new data types and send and receive data values.

You can [see it in action](http://quid2.org/app/ui).

Under the *Channels* tab are listed the currently open channels, every channel has a type and you can see its full definition by clicking on *Definition*.

Definitions are just [plain algebraic data types](https://github.com/Quid2/zm).

For example, you should see a *Message* channel that is used to implement a simple chat system. Click on *Show Values* to inspect the value being transferred and then use the [chat user interface](http://quid2.org/app/chat) to login and send a couple of messages and see them appear on the channel.

Under the *Types* tab is the list of types known to the system.

#### Minimalist but Evolvable

Top does not provide any other service beyond full-duplex typed communication, any other service (e.g. identification or encryption) has to be provided by the clients themselves but that can be done easily and independently by simply creating data types that stands for the additional functionality required.

### Usage

Using Top can be as simple as:

```haskell
{-# LANGUAGE DeriveGeneric ,DeriveAnyClass #-}
import Network.Top

-- |Send a message and then print out all messages received
main = runApp def ByType $ \conn -> do
  logLevel DEBUG
  output conn Message {fromUser="robin",content=TextMessage "Hello!"}
  loop conn
    where loop conn = input conn >>= print >> loop conn

-- Data model for a very simple chat system
data Message = Message {
     fromUser::String
    ,content::Content
    } deriving (Eq, Ord, Read, Show, Generic , Flat, Model)

data Content =
    TextMessage String

    | HTMLMessage String
    deriving (Eq, Ord, Read, Show, Generic, Flat, Model)
```
<sup>[Source Code](https://github.com/tittoassini/top-apps/blob/master/app/hello.hs)</sup>

For examples of stand-alone and www applications see:

* [top-apps-ghcjs](https://github.com/Quid2/top-apps-ghcjs)
  * Example WWW applications for [top](https://github.com/Quid2/top), using [ghcjs](https://github.com/ghcjs/ghcjs).
* [top-apps](https://github.com/Quid2/top-apps)
  * Example applications for [top](https://github.com/Quid2/top).

#### Installation

 Get the latest stable version from [hackage](https://hackage.haskell.org/package/top).

#### Compatibility

Tested with:
  * [ghc](https://www.haskell.org/ghc/) 7.10.3, 8.0.1 and 8.0.2 (x64)
  * [ghcjs](https://github.com/ghcjs/ghcjs)

So it can be used to develop both stand alone and WWW applications.

### The Top Service.

Though Top is eventually meant to develop into a distributed, vendor-free, protocol compatible with multiple transport protocols, its current implementation is provided by a single central router that supports [websockets](https://en.wikipedia.org/wiki/WebSocket) and therefore full-duplex, HTTP compatible channels.

TERMS OF SERVICE:
* The Top service is offered by Quid2 Limited - Registered in England and Wales - Reg No: 09213600.
* There is no charge for fair usage of the Top service. 
* Fair usage is defined as any usage that does not lead to a *de facto* denial of service to other users or that imposes unreasonable expense on its maintainer.
* By using the Top service you accept that the service is offered "as is" with no express or implied warranty for availability, performance, consistency, longevity or functionality.

