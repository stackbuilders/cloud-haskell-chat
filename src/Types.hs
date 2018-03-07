{-# LANGUAGE DeriveDataTypeable     #-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}

module Types where

import GHC.Generics
import Data.Binary
import Data.Typeable.Internal
import Data.Map (Map)
import Control.Distributed.Process (SendPort)


type ChatName = String

type NickName = String

type Host = String

type ServerAddress = String

type ClientPortMap = Map NickName (SendPort ChatMessage)

data Sender = Server | Client String
  deriving (Generic, Typeable, Eq, Show)

instance Binary Sender

data ChatMessage = ChatMessage {
    from :: Sender
  , message :: String
  } deriving (Generic, Typeable, Show)

instance Binary ChatMessage

newtype JoinChatMessage = JoinChatMessage {
    clientName :: NickName
  } deriving (Generic, Typeable, Show)

instance Binary JoinChatMessage
