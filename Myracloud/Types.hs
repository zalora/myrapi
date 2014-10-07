{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Myracloud.Types where

import GHC.Generics

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)

import Data.Aeson
import Data.Aeson.Types (defaultOptions, Options(..))

data ObjectVO a = ObjectVO { error :: Bool
                         , list :: [a]
                         , page :: Int
                         , count :: Int
                         , pageSize :: Int
                         } deriving (Show, Generic)

instance FromJSON a => FromJSON (ObjectVO a)

data ResultVO = ResultVO { result_error :: Bool
                         , result_violationList :: [ViolationVO]
                         , result_targetObject :: Value
                         } deriving (Show, Generic)

instance FromJSON ResultVO where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 7 })

data ViolationVO = ViolationVO { path :: Text
                               , message :: Text
                               } deriving (Show, Generic)

instance FromJSON ViolationVO

data DnsRecord = DnsRecord { id :: Int
                           , modified :: Text -- not really iso 8601
                           , created :: Text -- not really iso 8601
                           , name :: Text
                           , value :: Text
                           , ttl :: Int
                           , recordType :: Text
                           , active :: Bool

                           , priority :: Int -- not mentioned in the docs
                           , objectType :: Text -- not mentioned in the docs
                           } deriving (Show, Generic)

instance FromJSON DnsRecord
