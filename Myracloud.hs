{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module Myracloud where

import Data.Monoid (mconcat, mempty)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base64 as Base64
import Data.Byteable (toBytes)
import Data.Default

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A

import Data.Conduit (($$+-))
import qualified Data.Conduit as C
import qualified Data.Conduit.List as CL
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Resource (ResourceT, MonadThrow (throwM))
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import Data.Time.Clock
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

import Crypto.Hash (digestToHexByteString, hmacGetDigest, hash, hmac, HMAC, Digest, MD5, SHA512, SHA256)

type HTTPResponseConsumer a = HTTP.Response (C.ResumableSource (ResourceT IO) ByteString)
                              -> ResourceT IO a

consumeValue :: HTTPResponseConsumer Value
consumeValue res = HTTP.responseBody res $$+- CA.sinkParser A.json

class MyraHttp a where
  myraToHttp :: a -> HTTP.Request
  myraToHttp _ =
      def { HTTP.method = HTTP.methodGet
          , HTTP.secure = True
          , HTTP.host = "api.myracloud.com"
          , HTTP.port = 443
          , HTTP.path = "/en/rapi/DNSRecords/example.com/1"
          , HTTP.queryString = mempty
          , HTTP.requestHeaders = []
          , HTTP.requestBody = mempty
          , HTTP.decompress = HTTP.alwaysDecompress
          , HTTP.checkStatus = \_ _ _ -> Nothing
          }

class MyraHttp req => MyraCall req resp where
  consume :: req -> HTTPResponseConsumer resp
  default consume :: resp ~ Value => req -> HTTPResponseConsumer Value
  consume _ = consumeValue

data ListDNSRecords = ListDNSRecords ByteString

instance MyraHttp ListDNSRecords
instance MyraCall ListDNSRecords Value

iso8601 :: UTCTime -> ByteString
iso8601 = B8.pack . formatTime defaultTimeLocale "%FT%T%QZ"

myra :: MyraCall req resp => HTTP.Manager -> req -> IO resp
myra mgr req = do
    now <- getCurrentTime
    let iso = iso8601 now
        ht = myraToHttp req
        ht' = ht { HTTP.requestHeaders = ("Date", iso):(HTTP.requestHeaders ht) }
  
    undefined

data MyraSignature = MyraSignature { myra_rqBody :: Maybe ByteString
                                   , myra_method :: HTTP.Method
                                   , myra_uri :: ByteString
                                   , myra_contentType :: ByteString
                                   , myra_date :: ByteString -- "2014-04-26CET13:04:00+0100"
                                   }

hmacHex = digestToHexByteString . hmacGetDigest
md5 = digestToHexByteString . (hash :: ByteString -> Digest MD5)

myraSignature :: ByteString -> ByteString -> MyraSignature -> ByteString
myraSignature apiKey secret MyraSignature{..} = mconcat [ apiKey, ":", b64signature ]
  where
    b64signature = Base64.encode $ toBytes signature
    signature = hmac (toBytes signingKey) signingString :: HMAC SHA512

    key = mconcat [ "MYRA", secret ]
    dateKey = hmac key myra_date :: HMAC SHA256
    signingKey = hmac (toBytes dateKey) "myra-api-request" :: HMAC SHA256

    md5body = md5 $ maybe "" id myra_rqBody
    signingString = BS.intercalate "#" [ md5body, myra_method, myra_uri, myra_contentType, myra_date ]

