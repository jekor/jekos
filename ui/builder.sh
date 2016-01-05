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
cp js/bundle.js $out/bundle.js

cp -a img $out/
