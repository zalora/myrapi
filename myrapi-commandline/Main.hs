{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B8L
import           Data.Monoid
import qualified Data.Text as T
import           Myracloud
import           Myracloud.Commands
import           Myracloud.Commands.Util
import           Myracloud.Types hiding (value)
import           Options.Applicative
import           Servant.Common.BaseUrl
import           System.Exit
import           System.IO (stdout)

data Options = Options
  { optGlobalCredentials :: Credentials
  , optGlobalBaseUrl :: BaseUrl
  , optCommand :: Command
  } deriving (Show, Eq)

data Command = Create T.Text DnsRecordCreate
             | List DnsListOptions
             | Delete T.Text DnsRecordDelete
             | Update T.Text DnsRecordUpdate
             deriving (Show, Eq)

commandOptions :: Parser Command
commandOptions = subparser $
  (command "create" (info (Create <$> domainOption <*> dnsCreateOptions)
                     (progDesc "Create a record for a domain")))
 <>
  (command "list" (info (List <$> dnsListOptions)
                   (progDesc "List records for a domain")))
 <>
  (command "delete" (info (Delete <$> domainOption <*> dnsDeleteOptions)
                     (progDesc "Delete records for a domain")))
 <>
  (command "update" (info (Update <$> domainOption <*> dnsUpdateOptions)
                     (progDesc "Update records for a domain")))



globalOptions :: Parser Options
globalOptions = Options
  <$> credentialsOption
  <*> baseUrlOption
  <*> commandOptions

opts :: ParserInfo Options
opts = info (helper <*> globalOptions)
       (fullDesc <> progDesc "Command line interface to MYRACLOUD")

exit :: (Show a, A.ToJSON b) => Either a (Result b) -> IO ()
exit (Left x) = putStrLn ("ERROR: Failed with " <> show x) >> exitFailure
exit (Right x) = (B8L.hPutStrLn stdout $ A.encode x) >> case x of
  Myracloud.Types.Success _ -> exitSuccess
  Myracloud.Types.Failure _ -> exitFailure

wip :: IO ()
wip = putStrLn . mconcat $
  [ "Warning: there is something wrong with this feature, use"
  , "at your own risk and pay attention to the output!"
  ]

main :: IO ()
main = execParser opts >>= \case
  Options creds baseUrl com -> case com of
    Create s r -> runCreate creds r (Site s) baseUrl >>= exit
    List (DnsListOptions {..}) ->
      runList creds dnsListSite dnsListPage baseUrl >>= exit
    Delete s r -> wip >> runDelete creds r (Site s) baseUrl >>= exit
    Update s r -> wip >> runUpdate creds r (Site s) baseUrl >>= exit
