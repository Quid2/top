{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE DeriveGeneric  #-}
module Network.Quid2.Types(
  module Data.Typed
  ,Config(..)
  ,Connection(..),App
  ,def
  ,ByType(..)
  ,Echo(..)
  ,ByPattern(..)
--  ,module Data.Pattern
  ) where

import           Data.Default.Class
import           Data.Pattern hiding (Con,Var)
import           Data.Typed
import qualified Network.WebSockets       as WS

-- |General client configuration (ip,port and path of the quid2-net router)
data Config = Config {ip::String,port::Int,path::String}

-- TODO: point to failover ip
-- MAYBE: retrieve at a fixed address a list of servers' IP and then map the request by the 1-byte hash of the router type (eg. servers=[ip1,ip2] hash=x92 -> ip2)
-- MAYBE:use a fixed range of ips (say 256)
instance Default Config where def = Config "quid2.net" 8080 "/ws"

-- |A typed connection
data Connection a = Connection WS.Connection

 -- |An application that connects to a channel of type a and eventually returns an IO r
type App a r = Connection a -> IO r

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



