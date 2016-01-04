{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

import Blaze.ByteString.Builder (toByteString)
import Control.Concurrent (forkIO)
import Control.Monad (unless, when, void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Either (left)
import qualified Crypto.BCrypt as BC
import qualified Data.Aeson as A
import Data.Attoparsec.ByteString.Char8 (parseOnly, stringCI, skipSpace, takeWhile1)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (fromStrict, toStrict)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Data.Default (def)
import qualified Data.Digest.Pure.SHA as SHA
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import qualified Data.Text.IO as T
import Data.Time.Clock.POSIX (getPOSIXTime, posixDayLength)
import GHC.Generics
import Network.HTTP.Types (hAuthorization, hCookie, status200, status201, status303, status401, status403, status404, hContentType, methodOptions)
import Network (listenOn)
import Network.Wai
import Network.Wai.Middleware.Static (staticPolicy', initCaching, CachingStrategy(..), addBase, policy, (<|>))
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import Network.Wai.Handler.WebSockets (websocketsApp)
import qualified Network.WebSockets as WS
import qualified Network.TLS as TLS
import Servant
import System.Directory (doesFileExist)
import System.Environment (getArgs, getProgName)
import System.FilePath ((</>))
import System.IO (hPutStrLn, hPutStr, hPrint, stderr, withFile, IOMode(..))
import System.Posix.Pty (spawnWithPty, threadWaitReadPty, readPty, writePty)
import Web.Cookie (SetCookie(..), renderSetCookie, parseCookies)

import qualified Crypto.PubKey.HashDescr as C
import qualified Crypto.PubKey.RSA.PKCS15 as C
import qualified Crypto.Types.PubKey.RSA as C

import qualified Data.ASN1.BitArray as ASN
import qualified Data.ASN1.Encoding as ASN
import qualified Data.ASN1.BinaryEncoding as ASN
import qualified Data.ASN1.Types as ASN

-- TODO: If there are errors during initialization, we need to continue starting up and provide the list of errors on the interface front page. We can also get assistance resolving them from the installer (assuming we have at least managed to read the public key).

main = do
  args <- getArgs
  case args of
    [uiDir] -> do
      domain <- readFile "/var/jekos/domain"
      cache <- initCaching PublicStaticCaching
      authTokenKey <- withFile "/dev/urandom" ReadMode (\ h -> B.hGet h 32)
      runTLS (pems domain) (setPort 443 defaultSettings) (static cache uiDir (verifyAuth authTokenKey uiDir (upgradeWebsocket (rootApp uiDir))))
    _ -> do
      progName <- getProgName
      hPutStrLn stderr ("Usage: " ++ progName ++ " ui-dir")
 where static cache uiDir = staticPolicy' cache (addBase uiDir)
       pems domain = tlsSettingsChain (domainRoot domain </> "cert.pem") [domainRoot domain </> "fullchain.pem"] (domainRoot domain </> "key.pem")
       domainRoot domain = "/var/letsencrypt" </> domain

verifyAuth :: B.ByteString -> FilePath -> Middleware
verifyAuth authTokenKey uiDir app request sendResponse = do
  case (requestMethod request, rawPathInfo request) of
    ("POST", "/authtoken") -> do
      valid <- (validPassword . parseForm . toStrict) =<< lazyRequestBody request
      if valid
        then do
          token <- createAuthToken authTokenKey
          let cookie = toByteString (renderSetCookie (def { setCookieName = "auth"
                                                          , setCookieValue = token
                                                          , setCookiePath = Just "/"
                                                          , setCookieHttpOnly = True
                                                          , setCookieSecure = True }))
          sendResponse (responseLBS status303 [("Set-Cookie", cookie), ("Location", "/")] "")
        else authPlease "Invalid Password"
    ("DELETE", "/authtoken") -> do
      let cookie = toByteString (renderSetCookie (def { setCookieName = "auth"
                                                      , setCookieValue = ""
                                                      , setCookiePath = Just "/"
                                                      , setCookieMaxAge = Just 0 }))
      sendResponse (responseLBS status200 [("Set-Cookie", cookie)] "")
    ("OPTIONS", "/password") -> do
      sendResponse (responseLBS status200 (("Allow", "PUT") : corsHeaders) "")
    ("PUT", "/password") -> do
      -- Is this the initial password?
      hasPassword <- doesFileExist "/var/jekos/password"
      if hasPassword
        then sendResponse (responseLBS status403 corsHeaders "Password Already Set")
        else
          -- If so, instead of checking against the old password
          -- we're going to verify the signature of the new password
          -- with the public key we were given at boot.
          case lookup hAuthorization (requestHeaders request) of
            Nothing -> sigPlease "Request Signature Required"
            Just header ->
              -- Verify the signature.
              case parseOnly jekosSig header of
                Left err -> sigPlease "Invalid Signature Header"
                Right signature -> do
                  publicKey <- publicKeyFromDER `fmap` B.readFile "/var/jekos/publickey"
                  body <- toStrict `fmap` lazyRequestBody request
                  let url = rawPathInfo request
                      command = "PUT" `B.append` url `B.append` body
                  if C.verify C.hashDescrSHA256 publicKey command signature
                    then do
                      setPassword body
                      sendResponse (responseLBS status201 corsHeaders "")
                    else sigPlease "Invalid Signature"
    _ -> case lookupAuthToken of
           Just authToken -> do
             valid <- validAuthToken authTokenKey authToken
             if valid
               then case rawPathInfo request of
                      "/" -> do
                        mainPage <- BL.readFile (uiDir </> "index.html")
                        sendResponse (responseLBS status200 [(hContentType, "text/html; charset=utf-8")] mainPage)
                      _ -> app request sendResponse
               else authPlease "Invalid Auth Cookie"
           _ -> authPlease "Login"
 where sigPlease msg =
         sendResponse (responseLBS status401 ([ ("WWW-Authenticate", "JEKOS-SIG")
                                              , (hContentType, "text/plain") ] ++ corsHeaders) msg)
       jekosSig = do
         stringCI "JEKOS-SIG" >> skipSpace
         stringCI "signature" >> skipSpace
         stringCI "=" >> skipSpace
         base64 <- "\"" *> takeWhile1 (/= '"') <* "\""
         return (Base64.decodeLenient base64)
       lookupAuthToken = lookup "auth" . parseCookies =<< lookup hCookie (requestHeaders request)
       parseForm = (!! 1) . B8.split '='
       authPlease msg = do
         loginPage <- BL.readFile (uiDir </> "login.html")
         sendResponse (responseLBS status401 [("WWW-Authenticate", "Cookie realm=\"JekOS\" form-action=\"/authtoken\" cookie-name=auth"), (hContentType, "text/html; charset=utf-8")] loginPage)
       corsHeaders = [ ("Access-Control-Allow-Origin", "*")
                     , ("Access-Control-Allow-Headers", "Authorization")
                     , ("Access-Control-Allow-Methods", "PUT") ]

setPassword password = do
  hash' <- BC.hashPasswordUsingPolicy (BC.HashingPolicy 9 "$2y$") password
  case hash' of
    Nothing -> error "Failed to set password."
    Just hash -> B8.writeFile "/var/jekos/password" hash

validPassword password = do
  hash <- B8.readFile "/var/jekos/password"
  return (BC.validatePassword hash password)

createAuthToken authTokenKey = do
  expires <- (round . (+ posixDayLength)) `fmap` getPOSIXTime
  let signature = SHA.hmacSha256 (fromStrict authTokenKey) (fromStrict (B8.pack (show expires)))
  return (B8.pack (show expires ++ "." ++ show signature))

validAuthToken authTokenKey authToken = do
  now <- round `fmap` getPOSIXTime
  let [expires, mac] = B8.split '.' authToken
      signature = SHA.hmacSha256 (fromStrict authTokenKey) (fromStrict expires)
  return ((B8.pack (show signature)) == mac && now < read (B8.unpack expires))

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

upgradeWebsocket :: Middleware
upgradeWebsocket app request sendResponse =
  case (requestMethod request, rawPathInfo request, lookup "Upgrade" (requestHeaders request)) of
    ("GET", "/terminal", Just "websocket") ->
      case websocketsApp WS.defaultConnectionOptions terminalServer request of
        Nothing -> sendResponse (responseLBS status404 [] "Not Found")
        Just r -> sendResponse r
    _ -> app request sendResponse

-- TODO: Require password to be transmitted to start connection? (extra security)
terminalServer pending = do
  conn <- WS.acceptRequest pending
  WS.forkPingThread conn 30
  -- TODO: Get shell from /etc/passwd
  (pty, ph) <- spawnWithPty (Just [("TERM", "xterm")]) False "/run/current-system/sw/bin/bash" [] (80, 24)
  forkIO (writeToTerm pty conn)
  writeToSocket pty conn
 where writeToTerm pty conn = do
         bs <- WS.receiveData conn
         hPutStr stderr "received from socket: "
         hPrint stderr bs
         writePty pty bs
         writeToTerm pty conn
       writeToSocket pty conn = do
         threadWaitReadPty pty
         bs <- readPty pty
         hPutStr stderr "received from pty: "
         hPrint stderr bs
         WS.sendTextData conn bs
         writeToSocket pty conn

type RootAPI = "time" :> Get '[PlainText] Text

rootAPI :: Proxy RootAPI
rootAPI = Proxy

rootServer :: FilePath -> Server RootAPI
rootServer uiDir = time
 where time = (T.pack . show . round) `fmap` liftIO getPOSIXTime

rootApp :: FilePath -> Application
rootApp = serve rootAPI . rootServer
