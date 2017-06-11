{-# LANGUAGE CPP                       #-}
{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveFoldable            #-}
{-# LANGUAGE DeriveFunctor             #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings         #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift                #-}
{-# LANGUAGE StandaloneDeriving        #-}
#else
{-# LANGUAGE TemplateHaskell           #-}
#endif

module Network.Top.Types(
  -- *Top access point configuration
  Config(..),cfgIP,cfgPort,cfgPath,def

  -- *Connection Protocols
  --,byPattern
  ,ByPattern(..)
  ,ByType(..),byTypeRouter
  ,ByAny(..),byAny
  ,Echo(..)

  -- *Connection
  ,App
  ,Connection(..)
  ,inputWithTimeout

  -- *WebSocket Connection
  ,WSApp
  ,WSConnection
  ,chatsProtocol,chatsProtocolT

  -- *CHATS
  ,WSChannelResult
  ,ChannelSelectionResult(..)

  -- *Network Addresses
  ,WebSocketAddress(..),SocketAddress(..),IP4Address(..),IP6Address,HostAddress(..),HostPort(..)

  -- *Re-exports
  -- ,module ZM
  -- ,B.ByteString
  ) where

import qualified Data.ByteString                as B
import           Data.Default.Class
import           Data.List
import           Data.Pattern.Types
import           Data.Text                      (Text)
import qualified Data.Text                      as T
import           Data.Text.Encoding
import           Data.Word
import           Network.Top.Util
import           System.Timeout
import           Text.PrettyPrint.HughesPJClass (text)
import           ZM

#if __GLASGOW_HASKELL__ < 800
import           Language.Haskell.TH.Lift
#else
import           Language.Haskell.TH.Syntax (Lift) -- ,lift)
#endif

-- |Top's access point configuration
newtype Config = Config {accessPoint::WebSocketAddress IP4Address}

-- |Return Top's access point IP
cfgIP :: Config -> String
cfgIP = prettyShow . socketAddress . host . accessPoint

-- |Return Top's access point Port
cfgPort :: Config -> Int
cfgPort = fromIntegral . port . socketPort . host . accessPoint

-- |Return Top's access point Path
cfgPath :: Config -> String
cfgPath = path . accessPoint

-- |The configuration for the default Top router
instance Default Config where
  def = Config $ WebSocketAddress False (SocketAddress (DNSAddress "quid2.net") (HostPort 80)) "/ws"

---------------- Routing Protocols

-- |Return the value of the ByType router identifier for the given type
byTypeRouter :: Type AbsRef -> TypedBLOB
byTypeRouter t =
  let TypeApp f _ = absType (Proxy::Proxy (ByType ()))
  in typedBLOB_ (TypeApp f t) ByType

-- |Echo protocol: any value sent in is returned verbatim to the sender (useful for testing purposes)
-- Client can specify if received messages should be logged (for debugging purposes)
data Echo a = Echo { echoDebug :: Bool }
            deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (Echo a)

{-|
A routing protocol specified by a type.

Once a connection is established, clients:

   * can send messages of the given type

   * will receive all messages of the same type sent by other agents
-}
data ByType a = ByType
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a =>  Model (ByType a)

{-|
A routing protocol to receive all messages.

The ByAny type parameter indicates the type of the messages exchanged on the channel (usually:TypedBLOB).

Once a connection is established, clients:

   * can send messages of any type, as values of the ByAny type argument (for example: an Int value encoded as the corresponding TypedBLOB value)

   * will receive all messages sent by other agents
-}
data ByAny a = ByAny deriving (Eq, Ord, Show, Generic, Flat)
instance Model a =>  Model (ByAny a)

-- |Shortcut to specify
byAny :: ByAny TypedBLOB
byAny = ByAny :: ByAny TypedBLOB

---------- Connection

-- |An application that connects to a channel of type a and eventually returns an IO r
type App a r = Connection a -> IO r

-- |A typed bidirectional connection/channel
data Connection a = Connection {
   -- |Block read till a value is received
   input::IO a

   -- |Block write till a value is sent
   ,output::a -> IO ()

   -- |Close the connection
   ,close :: IO ()
 }

-- |Return a value received on the connection
-- or Nothing if no value is received in the specified number of seconds
--
-- NOTE: In case of timeout, the connection will be closed.
inputWithTimeout :: Int -> Connection a -> IO (Maybe a)
inputWithTimeout secs conn = timeout (seconds secs) (input conn)

---------- WebSocket Connection

-- |An application that connects to a WebSocket channel of type a and eventually returns an IO r
type WSApp r = App B.ByteString r

-- A typed connection
-- data Connection a = Connection WS.Connection

-- CHECK: use Input/Output from pipes-concurrency instead?
-- data WSConnection = WSConnection {sendMsg :: B.ByteString -> IO (),receiveMsg :: IO B.ByteString}
-- |A WebSocket connection
type WSConnection = Connection B.ByteString

-- data Connection a = Connection {
--    -- |Block read till a value is received
--    -- returns Nothing if the connection is closed
--    input::IO (Maybe a)

--    -- |Output a value
--    -- returns True if output succeeded, False otherwise
--    ,output::a -> IO Bool
--  }


---------- CHATS protocol

type WSChannelResult = ChannelSelectionResult (WebSocketAddress IP4Address)

-- |The value returned by an access point, after receiving a routing channel setup request.
data ChannelSelectionResult addr =
                                 -- |The channel has been permanently setup to the requested
                                 -- protocol
                                  Success
                                 |
                                 -- |The access point is unable or unwilling to open a connection
                                 -- with the requested routing protocol
                                  Failure { reason :: String }
                                 |
                                 -- |User should retry with the same transport protocol at the
                                 -- indicated address
                                  RetryAt addr
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ChannelSelectionResult a)

-- |CHATS binary identifier
chatsProtocol :: B.ByteString
chatsProtocol = encodeUtf8 chatsProtocolT

-- |CHATS textual identifier
chatsProtocolT :: T.Text
chatsProtocolT = "chats"

---------- Network Addresses

-- |The full address of a <https://en.wikipedia.org/wiki/WebSocket WebSocket> endpoint
data WebSocketAddress ip =
       WebSocketAddress
         {
         -- |True if the connection is wss (secure), False if is ws
         secure :: Bool
         -- |Host endpoint, example: SocketAddress (DNSAddress "quid2.net") (HostPort 80)
         , host :: SocketAddress ip
         -- |Path to the WebSocket access point, example: "/ws"
         , path :: String
         }
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model ip => Model (WebSocketAddress ip)

-- |The address of a <https://en.wikipedia.org/wiki/Network_socket network socket>
data SocketAddress ip = SocketAddress { socketAddress :: HostAddress ip, socketPort :: HostPort }
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model ip => Model (SocketAddress ip)

-- |A Sockets port (e.g. 80)
data HostPort = HostPort {port::Word16} deriving (Eq, Ord, Show, Generic,Flat,Model)

-- |A host address, either an IP or a DNS domain
data HostAddress ip = IPAddress ip
                    | DNSAddress String
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model ip => Model (HostAddress ip)

-- |An IP4 address
data IP4Address = IP4Address Word8 Word8 Word8 Word8 deriving (Eq, Ord, Show, Generic, Flat, Model)

-- |An IP6 address
data IP6Address = IP6Address Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16 deriving (Eq, Ord, Show, Generic, Flat, Model)

---------- Pretty instances
-- Easier to define here than in a separate file
instance Pretty IP4Address where
  pPrint (IP4Address w1 w2 w3 w4) = text . intercalate "." . map hex $ [w1, w2, w3, w4]

instance Pretty ip => Pretty (HostAddress ip) where
  pPrint (IPAddress ip) = pPrint ip
  pPrint (DNSAddress t) = text t


-- Lift instances (needed by TH)
#if __GLASGOW_HASKELL__ < 800
--deriveLift ''ByPattern
--deriveLift ''Match
deriveLift ''Pat
-- deriveLift ''WildCard
deriveLift ''PRef
#else
-- deriving instance Lift a => Lift (ByPattern a)
--deriving instance Lift (ByPattern a)
--deriving instance Lift Match
-- deriving instance Lift a => Lift (Type a)
-- deriving instance Lift AbsRef
-- deriving instance Lift (SHA3_256_6 a)
deriving instance Lift a => Lift (Pat a)
-- deriving instance Lift WildCard
deriving instance Lift PRef
#endif

-- Call/Return protocol
-- data Call a = Call a CallBack
--             | Return CallBack a
-- type CallBack = [Word8]

-- instance Invariant Conn where
--   invmap _ _ ConnOpening = ConnOpening
--   invmap f g (ConnOpen i o) = ConnOpen (fmap f i) (o . g)
--   invmap _ _ ConnClosed = ConnClosed


-- combine:: Connection a -> Connection b -> Connection (Either a b)
-- combine c1 c2 = Connection ci co
--   where
--     ci = do -- BAD: this blocks on the first connection
--       i1 <- input c1
--       case i1 of
--         Nothing -> (Right <$>) <$> input c2
--         Just v  -> return . Just . Left $ v
--     co (Left a)  = output c1 a
--     co (Right b) = output c2 b
