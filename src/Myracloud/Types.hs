{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module Myracloud.Types where

import           Control.Applicative
import           Data.Aeson hiding (Result)
import           Data.Aeson.Types (defaultOptions, Options(..))
import qualified Data.ByteString.Char8 as B8
import           Data.Proxy
import           Data.Text (Text)
import           GHC.Generics
import qualified Network.HTTP.Types as HTTP
import           Servant.API
import           Servant.Common.Text

data ObjectVO a = ObjectVO { error :: Bool
                         , list :: [a]
                         , page :: Int
                         , count :: Int
                         , pageSize :: Int
                         } deriving (Show, Generic)

instance FromJSON a => FromJSON (ObjectVO a)
instance ToJSON a => ToJSON (ObjectVO a)

data ResultVO = ResultVO { result_error :: Bool
                         , result_violationList :: [ViolationVO]
                         , result_targetObject :: Value
                         } deriving (Show, Generic)

instance FromJSON ResultVO where
  parseJSON = genericParseJSON (defaultOptions { fieldLabelModifier = drop 7 })
instance ToJSON ResultVO

data ViolationVO = ViolationVO { path :: Text
                               , message :: Text
                               } deriving (Show, Generic)

instance FromJSON ViolationVO
instance ToJSON ViolationVO

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

instance ToJSON DnsRecord
instance FromJSON DnsRecord

data DnsRecordCreate = DnsRecordCreate
  { _dnsrc_name :: Text
  , _dnsrc_value :: Text
  , _dnsrc_ttl :: Int
  , _dnsrc_recordType :: Text
  , _dnsrc_active :: Bool
  } deriving (Show, Eq, Generic)

instance ToJSON DnsRecordCreate where
  toJSON = genericToJSON $
           defaultOptions { fieldLabelModifier =
                               drop (length ("_dnsrc_" :: String)) }


instance FromJSON DnsRecordCreate where
  parseJSON = genericParseJSON $
              defaultOptions { fieldLabelModifier =
                                  drop (length ("_dnsrc_" :: String)) }

data DnsRecordUpdate = DnsRecordUpdate
  { _dnsup_id :: Int
  , _dnsup_modified :: Text
  , _dnsup_name :: Text
  , _dnsup_value :: Text
  , _dnsup_ttl :: Int
  , _dnsup_recordType :: Text
  , _dnsup_active :: Bool

  } deriving (Eq, Show, Generic)

instance ToJSON DnsRecordUpdate where
  toJSON = genericToJSON $
           defaultOptions { fieldLabelModifier =
                               drop (length ("_dnsup_" :: String)) }


instance FromJSON DnsRecordUpdate where
  parseJSON = genericParseJSON $
              defaultOptions { fieldLabelModifier =
                                  drop (length ("_dnsup_" :: String)) }

data DnsRecordDelete = DnsRecordDelete
  { _dnsdel_id :: Int
  , _dnsdel_modified :: Text
  } deriving (Show, Eq, Generic)

instance ToJSON DnsRecordDelete where
  toJSON = genericToJSON $
           defaultOptions { fieldLabelModifier =
                               drop (length ("_dnsdel_" :: String)) }


instance FromJSON DnsRecordDelete where
  parseJSON = genericParseJSON $
              defaultOptions { fieldLabelModifier =
                                  drop (length ("_dnsdel_" :: String)) }


newtype Site = Site { _unSite :: Text }
             deriving (Eq, Show, FromText, ToText)

newtype Date = Date { _unDate :: B8.ByteString }
             deriving (Eq, Show)

instance ToText Date where
  toText = toText . B8.unpack . _unDate

newtype Authorization = Authorization { _unAuthorization :: B8.ByteString }
                      deriving (Eq, Show)

instance ToText Authorization where
  toText = toText . B8.unpack . _unAuthorization


newtype ContentType = ContentType { _unContentType :: B8.ByteString }
                      deriving (Eq, Show)

instance ToText ContentType where
  toText = toText . B8.unpack . _unContentType


class GetMethod a where
   getMethod :: Proxy a -> B8.ByteString

instance GetMethod (Get x) where
   getMethod _ = HTTP.methodGet

instance GetMethod (Post x) where
   getMethod _ = HTTP.methodPost

instance GetMethod (Put x) where
   getMethod _ = HTTP.methodPut

instance GetMethod (Delete) where
   getMethod _ = HTTP.methodDelete

instance GetMethod b => GetMethod (a :> b) where
   getMethod _ = getMethod (Proxy :: Proxy b)

type MyraAccessKey = B8.ByteString
type MyraSecretKey = B8.ByteString
type Credentials = (MyraAccessKey, MyraSecretKey)
type Page = Int

data Result a = Failure Value | Success a
              deriving (Show, Eq, Generic, Functor)

instance FromJSON a => FromJSON (Result a) where
  parseJSON x = Myracloud.Types.Success <$> parseJSON x
                <|> pure (Failure x)

instance ToJSON a => ToJSON (Result a) where
  toJSON (Failure v) = v
  toJSON (Myracloud.Types.Success x) = toJSON x
