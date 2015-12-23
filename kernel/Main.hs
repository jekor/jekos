{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

import Control.Monad.IO.Class (liftIO)
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString.Char8 (parseOnly, stringCI, skipSpace, takeWhile1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime)
import GHC.Generics
import Network.Wai
import Network.Wai.Middleware.Static (staticPolicy', initCaching, CachingStrategy(..), addBase, policy, (<|>))
import Network.HTTP.Types (hAuthorization, status200, status401, hContentType, methodOptions)
import Network.Wai.Handler.Warp (run)
import Servant
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import qualified Crypto.PubKey.HashDescr as C
import qualified Crypto.PubKey.RSA.PKCS15 as C
import qualified Crypto.Types.PubKey.RSA as C

import qualified Data.ASN1.BitArray as ASN
import qualified Data.ASN1.Encoding as ASN
import qualified Data.ASN1.BinaryEncoding as ASN
import qualified Data.ASN1.Types as ASN

import Debug.Trace (traceShow)

main :: IO ()
main = do
  args <- getArgs
  case args of
    [uiDir] -> do
      -- We expect the public key to be available at a fixed location
      -- on the filesystem.  This needs to be done before we can serve
      -- requests (it's presumed to be handled by some startup script
      -- before this program is run).  The public key should be a
      -- DER-encoded RSA public key.
      publicKey <- publicKeyFromDER `fmap` B.readFile "/var/publickey"
      cache <- initCaching PublicStaticCaching
      run 13405 (static cache uiDir (corsAllowAll (verifySignature publicKey rootApp)))
    _ -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " ui-dir")
 where static cache uiDir = staticPolicy' cache (policy (indexFile uiDir) <|> addBase uiDir)
       indexFile uiDir s = traceShow s (if s == "" then Just (uiDir ++ "/index.html") else Nothing)

-- Read an RSA public key in DER format.
publicKeyFromDER :: B.ByteString -> C.PublicKey
publicKeyFromDER bs = case ASN.decodeASN1' ASN.DER bs of
    Left err -> error $ show err
    Right [ ASN.Start ASN.Sequence, ASN.Start ASN.Sequence
          , ASN.OID _, _, ASN.End ASN.Sequence
          , ASN.BitString s
          , ASN.End ASN.Sequence] -> case decodeBitArray s of
                                       Left err -> error $ show err
                                       Right as -> case ASN.fromASN1 as of
                                                     Left err -> error err
                                                     Right (x, []) -> x
                                                     _ -> error "unexpected extra data in DER public key"
    _ -> error "unexpected DER public key format"
  where
    decodeBitArray = ASN.decodeASN1' ASN.DER . ASN.bitArrayGetData

-- Authorization: JEKOS-SIG signature="sig"
verifySignature :: C.PublicKey -> Middleware
verifySignature publicKey app request sendResponse = do
  case lookup hAuthorization (requestHeaders request) of
    Nothing -> noAuth "Request signature is required."
    Just header ->
      case parseOnly jekosSig header of
        Left err -> noAuth "Invalid signature header."
        Right signature -> do
          body <- strictRequestBody request
          let method = encodeUtf8 (T.toLower (decodeUtf8 (requestMethod request)))
              url = rawPathInfo request
              command = method `B.append` url `B.append` toStrict (body)
          if C.verify C.hashDescrSHA256 publicKey command signature
            then app request sendResponse
            else noAuth "Invalid signature."
 where noAuth msg =
         sendResponse (responseLBS status401 [ (hContentType, "text/plain")
                                             , ("WWW-Authenticate", "JEKOS-SIG") ] msg)
       jekosSig = do
         stringCI "JEKOS-SIG" >> skipSpace
         stringCI "signature" >> skipSpace
         stringCI "=" >> skipSpace
         base64 <- "\"" *> takeWhile1 (/= '"') <* "\""
         return (Base64.decodeLenient base64)

-- Allow everything.
corsAllowAll :: Middleware
corsAllowAll = modifyResponse (mapResponseHeaders (++ corsHeaders)) .
               ifRequest optionsRequest (\ _ _ sendResponse -> sendResponse (responseLBS status200 [] ""))
 where corsHeaders = [ ("Access-Control-Allow-Origin", "*")
                     , ("Access-Control-Allow-Headers", "Authorization")]
       optionsRequest request = requestMethod request == methodOptions

type RootAPI = "time" :> Get '[PlainText] Text

rootAPI :: Proxy RootAPI
rootAPI = Proxy

rootServer :: Server RootAPI
rootServer = time
 where time = (T.pack . show . round) `fmap` liftIO getPOSIXTime

rootApp :: Application
rootApp = serve rootAPI rootServer
