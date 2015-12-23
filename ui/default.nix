{ stdenv, uglify, stylus }:

stdenv.mkDerivation {
  name = "jekos";
  src = ./.;
  buildInputs = [ uglify stylus ];
  builder = ./builder.sh;
}
