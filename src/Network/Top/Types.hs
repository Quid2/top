{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE DeriveLift #-}
{-# LANGUAGE StandaloneDeriving #-}
#else
{-# LANGUAGE TemplateHaskell #-}
#endif
module Network.Top.Types(
  module Data.Typed
  ,Config(..),cfgIP,cfgPort,cfgPath
  ,Connection(..),inputWithTimeout,App
  ,WSConnection,WSApp
  ,def
  ,ByType(..)
  ,ByAny(..),byAny
  ,Echo(..)
  --,ByPattern(..)
  --,byPattern
  ,L.ByteString,ChannelSelectionResult(..),WebSocketAddress(..),SocketAddress(..),IP4Address(..),IP6Address,HostAddress(..)
--  ,module Data.Pattern
  ,chatsProtocol,chatsProtocolT,WSChannelResult
  ) where

-- import Data.ByteString(ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy   as L
import           Data.Default.Class
-- import           Data.Functor.Invariant
import           Data.Pattern.Types
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Data.Typed
import           Data.Word
import           Data.Text.Encoding
import           System.Timeout
import           Network.Top.Util
#if __GLASGOW_HASKELL__ < 800
import           Language.Haskell.TH.Lift
#else
import           Language.Haskell.TH.Syntax (Lift,lift)
#endif
import           Text.PrettyPrint.HughesPJClass(text)
-- import           Data.Pattern.TH(asPattern)
import           Data.List
-- import Data.Pattern.Transform

-- |General client configuration (ip,port and path of the Top access point)
data Config = Config {accessPoint::WebSocketAddress IP4Address}

cfgIP :: Config -> String
cfgIP = prettyShow . socketAddress . host . accessPoint

cfgPort :: Config -> Int
cfgPort = fromIntegral . port . socketPort . host . accessPoint

cfgPath :: Config -> String
cfgPath = path . accessPoint

chatsProtocol :: B.ByteString
chatsProtocol = encodeUtf8 chatsProtocolT

chatsProtocolT :: T.Text
chatsProtocolT = "chats"

-- |The configuration for the default Top router
instance Default Config where
  def = Config $ WebSocketAddress False (SocketAddress (DNSAddress "quid2.net") (HostPort 80)) "/ws"

-- |A typed connection
-- data Connection a = Connection WS.Connection

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

-- NOTE: In case of timeout, this will cause the connection to close as well.
inputWithTimeout :: Int -> Connection a -> IO (Maybe a)
inputWithTimeout secs conn = timeout (seconds secs) (input conn)

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
data Echo a = Echo { echoDebug :: Bool }
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (Echo a)

-- |A router indexed by type:
-- Clients:
-- send messages of the given type
-- receive all messages of the same type sent by other agents
data ByType a = ByType
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a =>  Model (ByType a)


-- The type parameter indicates the type used to return the values (for example:TypedBLOB)
data ByAny a = ByAny deriving (Eq, Ord, Show, Generic, Flat)
instance Model a =>  Model (ByAny a)

byAny = ByAny :: ByAny TypedBLOB

data ChannelSelectionResult addr =
                                 -- |The channel has been permanently setup to the requested
                                 -- protocol
                                  Success
                                 |
                                 -- |The access point is unable or unwilling to open a connection
                                 -- with the requested routing protocol
                                  Failure { reason :: Text }
                                 |
                                 -- |User should retry with the same transport protocol at the
                                 -- indicated address
                                  RetryAt addr
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model a => Model (ChannelSelectionResult a)

type WSChannelResult = ChannelSelectionResult (WebSocketAddress IP4Address)

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

data HostPort = HostPort {port::Word16} deriving (Eq, Ord, Show, Generic,Flat,Model)

data HostAddress ip = IPAddress ip
                    | DNSAddress String
  deriving (Eq, Ord, Show, Generic, Flat)
instance Model ip => Model (HostAddress ip)

data IP4Address = IP4Address Word8 Word8 Word8 Word8 deriving (Eq, Ord, Show, Generic, Flat, Model)

data IP6Address = IP6Address Word16 Word16 Word16 Word16 Word16 Word16 Word16 Word16 deriving (Eq, Ord, Show, Generic, Flat, Model)

-- Easier to define here than in separate file
instance Pretty IP4Address where
  pPrint (IP4Address w1 w2 w3 w4) = text . intercalate "." . map hex $ [w1, w2, w3, w4]

instance Pretty ip => Pretty (HostAddress ip) where
  pPrint (IPAddress ip) = pPrint ip
  pPrint (DNSAddress t) = text t

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

