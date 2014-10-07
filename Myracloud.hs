{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Myracloud where

import Data.Monoid (mconcat, mempty)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as B8
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Base64 as Base64
import Data.Default

import Data.Aeson (Value)
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Encode.Pretty as A

import Data.Conduit (($$+-))
import qualified Data.Conduit as C
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, MonadThrow (throwM), runResourceT)
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import Data.Time.Clock
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

import Crypto.Hash (digestToHexByteString, hmacGetDigest, hash, hmac, HMAC, Digest, MD5, SHA512, SHA256)
import Data.Byteable (toBytes)

import System.Posix.Env.ByteString (getEnv)

type HTTPResponseConsumer a = HTTP.Response (C.ResumableSource (ResourceT IO) ByteString)
                              -> ResourceT IO a

consumeValue :: HTTPResponseConsumer Value
consumeValue res = HTTP.responseBody res $$+- CA.sinkParser A.json

defHttp =
    def { HTTP.method = HTTP.methodGet
        , HTTP.secure = True
        , HTTP.host = "api.myracloud.com"
        , HTTP.port = 443
        , HTTP.queryString = mempty
        , HTTP.requestHeaders = [("Content-Type", "application/json")]
        , HTTP.requestBody = mempty
        , HTTP.decompress = HTTP.alwaysDecompress
        }

class MyraHttp a where myraToHttp :: a -> HTTP.Request

class MyraHttp req => MyraCall req resp where
  consume :: req -> HTTPResponseConsumer resp
  default consume :: resp ~ Value => req -> HTTPResponseConsumer Value
  consume _ = consumeValue

data ListDNSRecords = ListDNSRecords ByteString

instance MyraHttp ListDNSRecords where
  myraToHttp (ListDNSRecords domain) = defHttp { HTTP.path = mconcat ["/en/rapi/dnsRecords/", domain ] }

instance MyraCall ListDNSRecords Value

iso8601 :: UTCTime -> ByteString
iso8601 = B8.pack . formatTime defaultTimeLocale "%FT%T%QZ"

data MyraSignature = MyraSignature { myra_rqBody :: Maybe ByteString
                                   , myra_method :: HTTP.Method
                                   , myra_uri :: ByteString
                                   , myra_contentType :: ByteString
                                   , myra_date :: ByteString
                                   } deriving (Show)

hmacHex = digestToHexByteString . hmacGetDigest
md5 = digestToHexByteString . (hash :: ByteString -> Digest MD5)

myraSignature :: ByteString -> ByteString -> MyraSignature -> ByteString
myraSignature apiKey secret MyraSignature{..} = mconcat [ "MYRA ", apiKey, ":", b64signature ]
  where
    md5body = md5 $ maybe "" id myra_rqBody
    signingString = BS.intercalate "#" [ md5body, myra_method, myra_uri, myra_contentType, myra_date ]

    dateKey = hmac (mconcat [ "MYRA", secret ]) myra_date :: HMAC SHA256
    signingKey = hmac (hmacHex dateKey) "myra-api-request" :: HMAC SHA256

    signature = hmac (hmacHex signingKey) signingString :: HMAC SHA512
    b64signature = Base64.encode $ toBytes . hmacGetDigest $ signature

myra :: MyraCall req resp => HTTP.Manager -> req -> ResourceT IO resp
myra mgr req = do
    now <- liftIO $ getCurrentTime
    Just access <- liftIO $ getEnv "MYRA_ACCESS_KEY"
    Just secret <- liftIO $ getEnv "MYRA_SECRET_KEY"
    let iso = iso8601 now
        ht = myraToHttp req
        sigData = MyraSignature { myra_rqBody = mempty
                                , myra_method = HTTP.method ht
                                , myra_uri = HTTP.path ht
                                , myra_contentType = maybe (error "no content-type") id
                                                     (lookup "Content-Type" $ HTTP.requestHeaders ht)
                                , myra_date = iso
                                }
        sig = myraSignature access secret sigData
        ht' = ht { HTTP.requestHeaders = ("Date", iso):("Authorization", sig):(HTTP.requestHeaders ht) }

    liftIO . print $ HTTP.requestHeaders ht'
    liftIO . print $ sigData

    hresp <- HTTP.http ht' mgr
    consume req hresp

myra_ :: MyraCall req Value => req -> IO ()
myra_ req = do
  HTTP.withManager $ \mgr -> do
    value :: Value <- myra mgr req
    liftIO $ L8.putStrLn $ A.encodePretty value

trySign = do
    Just access <- getEnv "MYRA_ACCESS_KEY"
    Just secret <- getEnv "MYRA_SECRET_KEY"
    let date = "2014-10-06T19:37:54.174591Z"
    print $ myraSignature access secret $ MyraSignature { myra_rqBody = Nothing
                                                        , myra_method = "GET"
                                                        , myra_uri = "/en/rapi/dnsRecords/zalora.sg"
                                                        , myra_contentType = "application/json"
                                                        , myra_date = date
                                                        }

