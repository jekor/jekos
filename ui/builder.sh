source $stdenv/setup

mkdir -p $out

cp -r $src src
chmod -R u+w src
pushd src

pushd css
stylus < login.styl > login.css
stylus < index.styl > index.css
cp index.css $out/
popd

# The login page is special as it'll be served out to unauthenticated clients.
sed -e '/{css}/{r css/login.css' -e 'd}' < login.html > $out/login.html

cp index.html $out/index.html

# pushd css

# cat <(stylus < common.styl) <(stylus < login.styl) <(stylus < appstore.styl) > $out/bundle.css

# popd

# pushd js

# For now I'm relying on npm to build bundle.js outside of Nix.
# When ready to try npm2nix, see node-packages-generated.nix in the
# top-level directory of nixpkgs for an example of how it seems to be
# used.

# uglifyjs bundle.js --mangle --screw-ie8 -o $out/bundle.js
# cp bundle.js $out/bundle.js
cp js/bundle.js $out/bundle.js

# popd

cp -a img $out/
