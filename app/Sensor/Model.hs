{-# Language DeriveGeneric #-}
module Sensor.Model where

import Data.Typed

-- A generic 
data SensorReading unit place = SensorReading unit place deriving (Eq, Ord, Read, Show, Generic)

-- A location that Google Maps can make sense of, such as: GoogleMapsLocation "The+White+House"
data GoogleMapsLocation = GoogleMapsLocation String
       deriving (Eq, Ord, Read, Show, Generic)

-- data Measure = TemperatureInCelsius Int
--              | Humidity Percentage
--              deriving (Eq, Ord, Read, Show, Generic)

-- This means just what it seems to mean
data Temperature t = Temperature t deriving (Eq, Ord, Read, Show, Generic)

data Celsius = Celsius Int deriving (Eq, Ord, Read, Show, Generic)

data Humidity = Humidity Percentage deriving (Eq, Ord, Read, Show, Generic)

-- |A 0..100 value
data Percentage = Percentage Int deriving (Eq, Ord, Read, Show, Generic)

instance Flat a => Flat (Temperature a)
instance Model a => Model (Temperature a)

instance (Flat a,Flat b) => Flat (SensorReading a b)
instance (Model a,Model b) => Model (SensorReading a b)

instance Flat Percentage
instance Model Percentage

instance Flat GoogleMapsLocation
instance Model GoogleMapsLocation


