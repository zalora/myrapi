module Myracloud.Util where

import           Control.Applicative
import           Control.Monad.IO.Class
import qualified Data.ByteString.Char8 as B8
import           Data.Time.Clock
import           Data.Time.Format (formatTime)
import           Data.Time.Locale.Compat (defaultTimeLocale)

iso8601 :: UTCTime -> B8.ByteString
iso8601 = B8.pack . formatTime defaultTimeLocale "%FT%T%QZ"

-- | Provides the current time in the 'iso8601' format.
currentTimestamp :: MonadIO m => m B8.ByteString
currentTimestamp = liftIO $ iso8601 <$> getCurrentTime
