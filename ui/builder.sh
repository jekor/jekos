source $stdenv/setup

mkdir $out
cd $src

cp index.html $out/index.html

pushd css

cat <(stylus < common.styl) <(stylus < appstore.styl) > $out/bundle.css

popd

pushd js

# For now I'm relying on npm to build bundle.js outside of Nix.
# When ready to try npm2nix, see node-packages-generated.nix in the
# top-level directory of nixpkgs for an example of how it seems to be
# used.

# uglifyjs bundle.js --mangle --screw-ie8 -o $out/bundle.js
cp bundle.js $out/bundle.js

popd
