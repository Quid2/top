{-# LANGUAGE DeriveGeneric #-}
module Chat.Model where

-- import Data.Model
import  Data.Typed

-- Data model for a simple chat system
data Message = Message {fromUser::User
                       ,subject::Subject
                       ,content::Content}
             deriving (Eq, Ord, Read, Show, Generic)

type User = String

-- Hierarchical subject
-- Example: Subject ["Haskell","Meeting","Firenze"]
data Subject = Subject [String] deriving (Eq, Ord, Read, Show, Generic)

-- Different kinds of contents
data Content =
             -- Basic text message
             TextMessage String

             -- Some extensions that we might implement during the meeting:

             -- Retrieve the subject being discussed
             | AskSubSubjects

             -- A system to track users that are following the current subject
             | Join
             | Leave
             | AskUsers      -- Ask for list of users 
             | Users [User]  -- Return list of users

             -- Retrieve recent messages
             | AskHistory         -- Ask
             | History [Message]  -- Return last messages

             -- Haskell queries
             | HaskellSearch String
             | HaskellExpression String

             -- and so on ...

             deriving (Eq, Ord, Read, Show, Generic)

-- We need to derive instances for Flat (binary serialisation so that our values can be tranferred on the network)
-- and Model (so that their structure can be picked up by the system)
-- both instances are derived automatically provided that our data type derive Generic
instance Flat Message
instance Flat Subject
instance Flat Content

instance Model Message
instance Model Subject
instance Model Content


