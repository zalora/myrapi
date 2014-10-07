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
import Data.Default

import Data.Aeson (Value, FromJSON, parseJSON)
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as A
import qualified Data.Aeson.Encode.Pretty as A

import Data.Conduit (($$+-))
import qualified Data.Conduit as C
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource (ResourceT, MonadThrow (throwM), runResourceT, MonadResource)
import qualified Data.Conduit.Attoparsec as CA

import qualified Network.HTTP.Types as HTTP
import qualified Network.HTTP.Conduit as HTTP

import Data.Time.Clock
import System.Locale (defaultTimeLocale)
import Data.Time.Format (formatTime)

import System.Posix.Env.ByteString (getEnv)

import qualified Myracloud.Types as MT
import Myracloud.Signature

type HTTPResponse m = HTTP.Response (C.ResumableSource m ByteString)
type HTTPResponseConsumer a = HTTPResponse (ResourceT IO) -> ResourceT IO a

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

type Page = Maybe Int

class MyraHttp a where myraToHttp :: a -> Page -> HTTP.Request

class MyraHttp req => MyraCall req resp where
  consume :: req -> HTTPResponseConsumer resp
  default consume :: resp ~ Value => req -> HTTPResponseConsumer Value
  consume _ = consumeValue

data ListDNSRecords = ListDNSRecords ByteString

instance MyraHttp ListDNSRecords where
  myraToHttp (ListDNSRecords domain) page =
    defHttp {
      HTTP.path = mconcat [
        "/en/rapi/dnsRecords/", domain, "/", B8.pack $ maybe "1" show page
        ]
      }

instance MyraCall ListDNSRecords Value

iso8601 :: UTCTime -> ByteString
iso8601 = B8.pack . formatTime defaultTimeLocale "%FT%T%QZ"

myra1 :: MonadResource m => HTTP.Request -> HTTP.Manager -> m (HTTPResponse m)
myra1 ht mgr = do
    now <- liftIO $ getCurrentTime
    Just access <- liftIO $ getEnv "MYRA_ACCESS_KEY"
    Just secret <- liftIO $ getEnv "MYRA_SECRET_KEY"
    let iso = iso8601 now
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

    HTTP.http ht' mgr

myra :: MyraCall req resp => HTTP.Manager -> req -> ResourceT IO resp
myra mgr req = do
    let ht = myraToHttp req Nothing
    hresp <- myra1 ht mgr
    consume req hresp

myra_ :: MyraCall req Value => req -> IO ()
myra_ req = do
  HTTP.withManager $ \mgr -> do
    value :: Value <- myra mgr req
    liftIO $ print $ (castValue value :: Maybe (MT.ObjectVO MT.DnsRecord))
  where
    castValue :: FromJSON a => Value -> Maybe a
    castValue v = parseMaybe (const (parseJSON v)) v


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

