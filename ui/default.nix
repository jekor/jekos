{ stdenv, uglify, stylus, gnused }:

stdenv.mkDerivation {
  name = "jekos";
  src = ./.;
  buildInputs = [ uglify stylus gnused ];
  builder = ./builder.sh;
}
