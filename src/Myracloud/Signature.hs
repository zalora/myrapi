{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Myracloud.Signature where

import Data.Monoid (mconcat)

import qualified Data.ByteString as BS
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base64 as Base64

import Crypto.Hash (digestToHexByteString, hmacGetDigest, hash, hmac, HMAC, Digest, MD5, SHA512, SHA256)
import Data.Byteable (toBytes)

import qualified Network.HTTP.Types as HTTP

data MyraSignature = MyraSignature { myra_rqBody :: Maybe ByteString
                                   , myra_method :: HTTP.Method
                                   , myra_uri :: ByteString
                                   , myra_contentType :: ByteString
                                   , myra_date :: ByteString
                                   } deriving (Show, Eq)

hmacHex :: HMAC a -> ByteString
hmacHex = digestToHexByteString . hmacGetDigest

md5 :: ByteString -> ByteString
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
