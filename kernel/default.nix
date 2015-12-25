{ mkDerivation, aeson, asn1-encoding, asn1-types, attoparsec, base
, base64-bytestring, bytestring, containers, crypto-pubkey
, crypto-pubkey-types, filepath, http-types, servant-server, stdenv
, text, time, transformers, wai, wai-cors, wai-middleware-static
, warp, warp-tls
}:
mkDerivation {
  pname = "kernel";
  version = "0.1.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    aeson asn1-encoding asn1-types attoparsec base base64-bytestring
    bytestring containers crypto-pubkey crypto-pubkey-types filepath
    http-types servant-server text time transformers wai wai-cors
    wai-middleware-static warp warp-tls
  ];
  license = stdenv.lib.licenses.unfree;
}
