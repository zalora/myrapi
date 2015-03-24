module Myracloud.Commands.Util where

import           Control.Applicative
import qualified Data.ByteString.Char8 as B8
import           Data.Char (toLower)
import           Data.Monoid
import qualified Data.Text as T
import           Myracloud.Types hiding (value)
import           Options.Applicative
import           Servant.Common.BaseUrl

domainOption :: Parser T.Text
domainOption = textOption
               ( long "domain"
              <> metavar "DOMAIN"
              <> help "domain name we're querying")

subdomainOption :: Parser T.Text
subdomainOption = textOption
               ( long "subdomain"
              <> metavar "SUBDOMAIN"
              <> help "subdomain name we're querying")

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

optionalPage :: Parser (Maybe Page)
optionalPage = option (return <$> auto)
               ( long "page"
                 <> metavar "PAGE"
                 <> help ("page number of the list of records, e.g. 1;"
                          ++ " checks all pages if unspecified")
                 <> value Nothing)

byteStringOption :: Mod OptionFields String -> Parser B8.ByteString
byteStringOption x = B8.pack <$> strOption x

textOption :: Mod OptionFields String -> Parser T.Text
textOption x = T.pack <$> strOption x

siteOption :: Mod OptionFields String -> Parser Site
siteOption x = Site <$> textOption x
