module Myracloud.Commands.DNS where

import qualified Data.Text as T
import           Myracloud.Commands.Util
import           Myracloud.Types hiding (value)
import           Options.Applicative


data DnsListOptions = DnsListOptions
  { dnsListSite :: Site
  , dnsListPage :: Int
  } deriving (Show, Eq)


domainOption :: Parser T.Text
domainOption = textOption
               ( long "domain"
              <> metavar "DOMAIN"
              <> help "domain name we're querying")

dnsListOptions :: Parser DnsListOptions
dnsListOptions = (<*>) helper $ DnsListOptions
  <$> siteOption
      ( long "domain"
     <> metavar "DOMAIN"
     <> help "domain to query the records for" )
  <*> option auto
      ( long "page"
     <> metavar "PAGE"
     <> help "page number of the list of records"
     <> value 1
     <> showDefault)

dnsCreateOptions :: Parser DnsRecordCreate
dnsCreateOptions = (<*>) helper $ DnsRecordCreate
  <$> textOption
    ( long "subdomain"
   <> metavar "SUBDOMAIN"
   <> help "name to create a record for, e.g. sub.example.com" )
  <*> textOption
    ( long "value"
   <> metavar "VALUE"
   <> help "value for the record, e.g. 127.0.0.1")
  <*> option auto
    ( long "ttl"
   <> metavar "TTL"
   <> help "TTL for the record, e.g. 300")
  <*> textOption
    ( long "type"
   <> metavar "TYPE"
   <> help "record type, e.g. A")
  <*> switch
    ( long "active"
   <> help "whether the record should be made active")
