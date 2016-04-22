{-# Language DeriveGeneric #-}
module Sensor.Model1 where

import Data.Typed

-- A rather asinine data model
data MySensor = MySensor Int -- These are Celsius by the way!
              deriving (Eq, Ord, Read, Show, Generic)

instance Flat MySensor
instance Model MySensor
