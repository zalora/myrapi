{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

module Myracloud where

import           Control.Monad.Trans.Either
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Monoid
import           Data.Proxy
import           Data.Text
import           Myracloud.Signature
import           Myracloud.Types
import           Myracloud.Util
import           Servant.API
import           Servant.Client

type DnsListApi = "en" :> "rapi" :> "dnsRecords"
                  :> Capture "site" Site
                  :> Capture "page" Page
                  :> Header "Date" Date
                  :> Header "Authorization" Authorization
                  :> Header "Content-Type" ContentType
                  :> Get (ObjectVO DnsRecord)

dnsListApi :: Proxy DnsListApi
dnsListApi = Proxy

-- | List the available Dns records.
listRecords :: Credentials
            -> Site -- ^ Site to list the records for
            -> Page
            -> BaseUrl -- ^ details of the server, such as 'myraUri'
            -> EitherT String IO (ObjectVO DnsRecord)
listRecords (access, secret) site@(Site s') page' base = do
  iso <- currentTimestamp
  let contentType = ContentType "application/json"
      sigData = MyraSignature
        { myra_rqBody = mempty
        , myra_method = getMethod dnsListApi
        , myra_uri = "/en/rapi/dnsRecords/"
                     <> B8.pack (unpack s')
                     <> "/"
                     <> B8.pack (show page')
        , myra_contentType = _unContentType contentType
        , myra_date = iso
        }
      sig = myraSignature access secret sigData

  client dnsListApi site page'
    (Just $ Date iso)
    (Just $ Authorization sig)
    (Just contentType)
    base

runList :: Credentials -> Site -> Page -> BaseUrl
        -> IO (Either String (ObjectVO DnsRecord))
runList c s p b = runEitherT $ listRecords c s p b

type DnsCreateApi = "en" :> "rapi" :> "dnsRecords"
                    :> Capture "site" Site
                    :> Header "Date" Date
                    :> Header "Authorization" Authorization
                    :> Header "Content-Type" ContentType
                    :> ReqBody DnsRecordCreate
                    :> Put ResultVO

dnsCreateApi :: Proxy DnsCreateApi
dnsCreateApi = Proxy

createRecord :: Credentials -> DnsRecordCreate -> Site -> BaseUrl
          -> EitherT String IO ResultVO
createRecord (access, secret) r site@(Site s') b = do
  iso <- currentTimestamp
  let contentType = ContentType "application/json"
      sigData = MyraSignature
        { myra_rqBody = Just . BL.toStrict $ A.encode r
        , myra_method = getMethod dnsCreateApi
        , myra_uri = "/en/rapi/dnsRecords/" <> B8.pack (unpack s')
        , myra_contentType = _unContentType contentType
        , myra_date = iso
        }
      sig = myraSignature access secret sigData

  client dnsCreateApi
    site
    (Just $ Date iso)
    (Just $ Authorization sig)
    (Just contentType) r b

runCreate :: Credentials -> DnsRecordCreate -> Site -> BaseUrl
          -> IO (Either String ResultVO)
runCreate c r s b = runEitherT $ createRecord c r s b
