{ stdenv, bash
}:


stdenv.mkDerivation {
  name = "jekos.org";
  src = ./.;
  buildInputs = [ bash ];
  inherit bash;
  builder = ./builder.sh;
}
