{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Myracloud.DNS where

import           Control.Applicative ((<$>))
import           Control.Monad.Trans.Either
import qualified Data.Aeson as A
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.Function (on)
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
                  :> Get (Result (ObjectVO DnsRecord))

dnsListApi :: Proxy DnsListApi
dnsListApi = Proxy

-- | List the available Dns records.
listRecords :: Credentials
            -> Site -- ^ Site to list the records for
            -> Page
            -> BaseUrl -- ^ details of the server, such as 'myraUri'
            -> EitherT String IO (Result (ObjectVO DnsRecord))
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
        -> IO (Either String (Result (ObjectVO DnsRecord)))
runList c s p b = runEitherT $ listRecords c s p b

-- | Runs the list command on consecutive pages until no more records
-- are returned.
runListAll :: Credentials -> Site -> BaseUrl
           -> IO (Either String (Result [DnsRecord]))
runListAll c s b = runEitherT $ runListAll' c s b

runListAll' :: Credentials -> Site -> BaseUrl
            -> EitherT String IO (Result [DnsRecord])
runListAll' c s b = loop 1
  where
    loop :: Page -> EitherT String IO (Result [DnsRecord])
    loop p = listRecords c s p b >>= \case
      Failure x -> return $ Failure x
      Success x -> case list x of
        [] -> return $ Success []
        xs | pageSize x > Prelude.length xs -> return $ Success xs
           | otherwise -> fmap (xs ++) <$> loop (succ p)

type DnsCreateApi = "en" :> "rapi" :> "dnsRecords"
                    :> Capture "site" Site
                    :> Header "Date" Date
                    :> Header "Authorization" Authorization
                    :> Header "Content-Type" ContentType
                    :> ReqBody DnsRecordCreate
                    :> Put (Result ResultVO)

dnsCreateApi :: Proxy DnsCreateApi
dnsCreateApi = Proxy

createRecord :: Credentials -> DnsRecordCreate -> Site -> BaseUrl
             -> EitherT String IO (Result ResultVO)
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
          -> IO (Either String (Result ResultVO))
runCreate c r s b = runEitherT $ createRecord c r s b

{- TODO: Hack until servant-client allows for body parsing from Delete -}
type DeleteResult = Result () {- ResultVO -}

type DnsDeleteApi = "en" :> "rapi" :> "dnsRecords"
                    :> Capture "site" Site
                    :> Header "Date" Date
                    :> Header "Authorization" Authorization
                    :> Header "Content-Type" ContentType
                    :> ReqBody DnsRecordDelete
                    :> Delete {-Post (Result ResultVO)-}

dnsDeleteApi :: Proxy DnsDeleteApi
dnsDeleteApi = Proxy

deleteRecord :: Credentials -> DnsRecordDelete -> Site -> BaseUrl
             -> EitherT String IO DeleteResult
deleteRecord (access, secret) r site@(Site s') b = do
  iso <- currentTimestamp
  let contentType = ContentType "application/json"
      sigData = MyraSignature
        { myra_rqBody = Just . BL.toStrict $ A.encode r
        , myra_method = getMethod dnsDeleteApi
        , myra_uri = "/en/rapi/dnsRecords/" <> B8.pack (unpack s')
        , myra_contentType = _unContentType contentType
        , myra_date = iso
        }
      sig = myraSignature access secret sigData

  {- TODO: Hack until servant-client allows for body parsing from Delete -}
  r <- client dnsDeleteApi
    site
    (Just $ Date iso)
    (Just $ Authorization sig)
    (Just contentType) r b
  return $ Success ()

runDelete :: Credentials -> DnsRecordDelete -> Site -> BaseUrl
          -> IO (Either String DeleteResult)
runDelete c r s b = runEitherT $ deleteRecord c r s b

type DnsUpdateApi = "en" :> "rapi" :> "dnsRecords"
                    :> Capture "site" Site
                    :> Header "Date" Date
                    :> Header "Authorization" Authorization
                    :> Header "Content-Type" ContentType
                    :> ReqBody DnsRecordUpdate
                    :> Post (Result ResultVO)

dnsUpdateApi :: Proxy DnsUpdateApi
dnsUpdateApi = Proxy

updateRecord :: Credentials -> DnsRecordUpdate -> Site -> BaseUrl
             -> EitherT String IO (Result ResultVO)
updateRecord (access, secret) r site@(Site s') b = do
  iso <- currentTimestamp
  let contentType = ContentType "application/json"
      sigData = MyraSignature
        { myra_rqBody = Just . BL.toStrict $ A.encode r
        , myra_method = getMethod dnsUpdateApi
        , myra_uri = "/en/rapi/dnsRecords/" <> B8.pack (unpack s')
        , myra_contentType = _unContentType contentType
        , myra_date = iso
        }
      sig = myraSignature access secret sigData

  client dnsUpdateApi
    site
    (Just $ Date iso)
    (Just $ Authorization sig)
    (Just contentType) r b

runUpdate :: Credentials -> DnsRecordUpdate -> Site -> BaseUrl
          -> IO (Either String (Result ResultVO))
runUpdate c r s b = runEitherT $ updateRecord c r s b

search :: Credentials -> Site -> BaseUrl
       -> Maybe Page
       -> Site -- ^ Subdomain to search for
       -> IO (Either String (Result [DnsRecord]))
search c s b p (Site sub) = runEitherT $ searchBy' c s b p ((== sub) . name)

searchBy' :: Credentials -> Site -> BaseUrl
          -> Maybe Page -- ^ Specific page to focus the search on, if any
          -> (DnsRecord -> Bool) -- ^ 'Prelude.filter' predicate
          -> EitherT String IO (Result [DnsRecord])
searchBy' c s b p rpred = go p >>= \case
  Success xs -> return . Success $ Prelude.filter rpred xs
  x -> return x
  where
    go Nothing = runListAll' c s b
    go (Just p) = fmap list <$> listRecords c s p b
