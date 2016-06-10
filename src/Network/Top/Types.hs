{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric ,OverloadedStrings #-}
module Network.Top.Types(
  module Data.Typed
  ,Config(..),cfgIP,cfgPort,cfgPath
  ,Connection(..),App--,WSConnection(..),WSApp
  ,WSConnection,WSApp
  ,def
  ,ByType(..)
  ,Echo(..)
  ,ByPattern(..),L.ByteString
--  ,module Data.Pattern
  ) where

import qualified Data.ByteString.Lazy   as L
import           Data.Default.Class
import           Data.Functor.Invariant
import           Data.Pattern           hiding (Con, Var)
import           Data.Typed
import           Data.Text(Text)
import  qualified         Data.Text as T
import           Data.Word
import qualified Network.WebSockets     as WS

-- |General client configuration (ip,port and path of the Top access point)
-- data Config = Config {ip::String,port::Int,path::String}
data Config = Config {accessPoint::WebSocketAddress IP4Address}

cfgIP :: Config -> String
cfgIP = prettyShow . socketAddress . host . accessPoint

cfgPort :: Config -> Int
cfgPort = fromIntegral . port . socketPort . host . accessPoint

cfgPath :: Config -> String
cfgPath = T.unpack . path . accessPoint

-- TODO: point to failover ip
-- MAYBE: retrieve at a fixed address a list of servers' IP and then map the request by the 1-byte hash of the router type (eg. servers=[ip1,ip2] hash=x92 -> ip2)
-- MAYBE:use a fixed range of ips (say 256)
-- instance Default Config where def = Config "quid2.net" 8080 "/ws"
instance Default Config where def = Config (WebSocketAddress False (SocketAddress (DNSAddress "quid2.net") (HostPort 8080)) "/ws")
-- instance Default Config where def = Config "127.0.0.1" 8080 "/ws"

-- |A typed connection
 -- data Connection a = Connection WS.Connection
-- data Connection a = Connection WSConnection

-- CHECK: use Input/Output from pipes-concurrency instead?
-- data WSConnection = WSConnection {sendMsg :: L.ByteString -> IO (),receiveMsg :: IO L.ByteString}
type WSConnection = Connection L.ByteString

-- data Connection a = Connection {
--    -- |Block read till a value is received
--    -- returns Nothing if the connection is closed
--    input::IO (Maybe a)

--    -- |Output a value
--    -- returns True if output succeeded, False otherwise
--    ,output::a -> IO Bool
--  }

data Connection a = Connection {
   -- |Block read till a value is received
   input::IO a

   -- |Block write till a value is sent
   ,output::a -> IO ()

   -- |Close the connection
   ,close :: IO ()
 }


-- instance Invariant Conn where
--   invmap _ _ ConnOpening = ConnOpening
--   invmap f g (ConnOpen i o) = ConnOpen (fmap f i) (o . g)
--   invmap _ _ ConnClosed = ConnClosed

-- |An application that connects to a channel of type a and eventually returns an IO r
type App a r = Connection a -> IO r

-- |An application that connects to a WebSocket channel of type a and eventually returns an IO r
type WSApp r = App L.ByteString r

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

---------------- Routers

-- |Echo router: any value sent in is returned verbatim to the sender (for testing purposes only)
-- Client can specify if received messages should be logged (for debugging purposes)
data Echo a = Echo {echoDebug::Bool} deriving (Eq, Ord ,Show ,Generic)
instance Flat (Echo a)
instance Model a => Model (Echo a)

-- |A router indexed by type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type sent by other agents
data ByType a = ByType deriving (Eq, Ord, Show, Generic)

instance Flat (ByType a)
instance Model a =>  Model (ByType a)

-- |A router index by a pattern of a given type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type, that match the given pattern, sent by other agents
-- NOTE: not yet implemented
data ByPattern a = ByPattern (Pattern WildCard) deriving (Eq, Ord, Show, Generic)

instance Flat (ByPattern a)
instance Model a => Model (ByPattern a)

data RoutingSelectionResult addr =
  -- |The channel has been permanently setup to the requested routing protocol
  Success
  -- |The access point is unable or unwilling to open a connection with the requested routing protocol
  | Failure {reason::Text}
  -- |User should retry with the same transport protocol at the indicated address
  | RetryAt addr
  deriving (Eq, Ord, Show, Generic)

data WebSocketAddress ip = WebSocketAddress {
  -- |True if the connection is wss, False if is ws
  secure::Bool
  -- |Host endpoint, example: EndPoint (DNSAddress "quid2.net") (HostPort 8080)
  ,host::SocketAddress ip
  -- |Path to the WebSocket entry point, example: "/ws"
  ,path::Text
  } deriving (Eq, Ord, Show, Generic)
instance Flat ip => Flat (WebSocketAddress ip)
instance Model ip => Model (WebSocketAddress ip)

data SocketAddress ip = SocketAddress {socketAddress::HostAddress ip,socketPort::HostPort} deriving (Eq, Ord, Show, Generic)
instance Flat ip => Flat (SocketAddress ip)
instance Model ip => Model (SocketAddress ip)

data HostPort = HostPort {port::Word16} deriving (Eq, Ord, Show, Generic)
instance Flat HostPort
instance Model HostPort

data HostAddress ip =
  IPAddress ip
  | DNSAddress Text
  deriving (Eq, Ord, Show, Generic)
instance Flat ip => Flat (HostAddress ip)
instance Model ip => Model (HostAddress ip)

instance Pretty ip => Pretty (HostAddress ip) where
  pPrint (IPAddress ip) = pPrint ip
  pPrint (DNSAddress t) = txt t

data IP4Address = IP4Address Word8 Word8 Word8 Word8 deriving (Eq, Ord, Show, Generic)
instance Flat IP4Address
instance Model IP4Address

instance Pretty IP4Address where pPrint (IP4Address w1 w2 w3 w4) = let dot = T.singleton '.' in txt . T.intercalate dot . map tshow $ [w1,w2,w3,w4]

data IP6Address = IP6Address Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16 deriving (Eq, Ord, Show, Generic)
instance Flat IP6Address
instance Model IP6Address



