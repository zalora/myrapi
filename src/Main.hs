{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.ByteString.Char8 as B8
import           Data.Char (toLower)
import           Data.Monoid
import qualified Data.Text as T
import           Myracloud
import           Myracloud.Types hiding (value)
import           Options.Applicative
import           Servant.Common.BaseUrl
import           System.Exit

data Options = Options
  { optGlobalCredentials :: Credentials
  , optGlobalBaseUrl :: BaseUrl
  , optCommand :: Command
  } deriving (Show, Eq)

data DnsListOptions = DnsListOptions
  { dnsListSite :: Site
  , dnsListPage :: Int
  } deriving (Show, Eq)

data Command = Create T.Text DnsRecordCreate
             | List DnsListOptions
             deriving (Show, Eq)

credentialsOption :: Parser Credentials
credentialsOption = (,)
  <$> byteStringOption
      ( long "access-key"
     <> metavar "ACCESS"
     <> help "API access key")
  <*> byteStringOption
      ( long "secret-key"
     <> metavar "SECRET"
     <> help "API secret key")

schemeOption :: Parser Scheme
schemeOption = option schemeReadM
               ( long "scheme"
              <> metavar "SCHEME"
              <> help "scheme to use, either http or https"
              <> value Https
              <> showDefaultWith (map toLower . show))
  where
    schemeReadM :: ReadM Scheme
    schemeReadM = str >>= \s -> case toLower <$> s of
      "http" -> return Http
      "https" -> return Https
      _ -> readerError $
           "‘" ++ s ++ "’ is not a valid scheme, use http or https instead"

baseUrlOption :: Parser BaseUrl
baseUrlOption = BaseUrl
  <$> schemeOption
  <*> strOption
    ( long "host"
   <> metavar "HOST"
   <> help "API server address"
   <> value "api.myracloud.com"
   <> showDefaultWith Prelude.id)
  <*> option auto
    ( long "port"
   <> metavar "PORT"
   <> help "port to use for the server"
   <> value 443
   <> showDefault)

byteStringOption :: Mod OptionFields String -> Parser B8.ByteString
byteStringOption x = B8.pack <$> strOption x

textOption :: Mod OptionFields String -> Parser T.Text
textOption x = T.pack <$> strOption x

siteOption :: Mod OptionFields String -> Parser Site
siteOption x = Site <$> textOption x

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

domainOption :: Parser T.Text
domainOption = textOption
               ( long "domain"
              <> metavar "DOMAIN"
              <> help "domain name we're querying")

commandOptions :: Parser Command
commandOptions = subparser $
 (command "create" (info (Create <$> domainOption <*> dnsCreateOptions)
                     (progDesc "Create a record for a domain")))
  <>
   (command "list" (info (List <$> dnsListOptions)
                    (progDesc "List records for a domain")))

globalOptions :: Parser Options
globalOptions = Options
  <$> credentialsOption
  <*> baseUrlOption
  <*> commandOptions

opts :: ParserInfo Options
opts = info (helper <*> globalOptions)
       (fullDesc <> progDesc "Command line interface to MYRACLOUD")

exit :: (Show a, Show b) => Either a b -> IO ()
exit (Left x) = putStrLn ("ERROR: Failed with " <> show x) >> exitFailure
exit (Right x) = print x >> exitSuccess

main :: IO ()
main = execParser opts >>= \case
  Options creds baseUrl com -> case com of
    Create s r -> runCreate creds r (Site s) baseUrl >>= exit
    List (DnsListOptions {..}) ->
      runList creds dnsListSite dnsListPage baseUrl >>= exit
