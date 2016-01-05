source $stdenv/setup

cp -r $src/www $out
chmod +w $out
mkdir $out/aws-sdk
cp $awssdk $out/aws-sdk/application.javascript
