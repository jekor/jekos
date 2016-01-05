{ stdenv, fetchurl, bash }:

stdenv.mkDerivation {
  name = "jekos.org";
  src = ./.;
  buildInputs = [ bash ];
  inherit bash;
  builder = ./builder.sh;
  awssdk = fetchurl {
    url = "https://raw.githubusercontent.com/aws/aws-sdk-js/47f6e3d77b58997d8e5af6c80a0709280c4c54dc/dist/aws-sdk.min.js";
    sha256 = "188wd2dbipqw3br6hdv3yfscnmgyc732scjmaf09gff1hzcxp7rm";
  };
}
