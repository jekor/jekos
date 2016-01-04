{ mkDerivation, aeson, asn1-encoding, asn1-types, attoparsec, base
, base64-bytestring, bcrypt, blaze-builder, bytestring, containers
, cookie, crypto-pubkey, crypto-pubkey-types, data-default
, directory, either, filepath, http-types, network, posix-pty
, servant-server, SHA, stdenv, text, time, tls, transformers, wai
, wai-cors, wai-middleware-static, wai-websockets, warp, warp-tls
, websockets
}:
mkDerivation {
  pname = "kernel";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson asn1-encoding asn1-types attoparsec base base64-bytestring
    bcrypt blaze-builder bytestring containers cookie crypto-pubkey
    crypto-pubkey-types data-default directory either filepath
    http-types network posix-pty servant-server SHA text time tls
    transformers wai wai-cors wai-middleware-static wai-websockets warp
    warp-tls websockets
  ];
  license = stdenv.lib.licenses.unfree;
}
