let
  region = "us-west-2";
  pkgs = import ~/code/nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
  hpkgs = pkgs.haskell.packages.ghc7102;
  jw = hpkgs.callPackage <jsonwrench> { };
  fetch-user-data = builtins.toFile "fetch-user-data.sh" ''
    set -euo pipefail
    coreutils=$1
    curl=$2
    jw=$3
    mkdir -p /var/jekos
    cd /var/jekos
    # First try to lookup the subdomain key. If the user-data is not
    # JSON (as in the case with nixops), we want to bail out as soon
    # as possible (before storing incorrect data on the filesystem).
    $curl/bin/curl http://169.254.169.254/latest/user-data | $jw/bin/jw lookup subdomain
    $curl/bin/curl -o user-data.json http://169.254.169.254/latest/user-data
    $jw/bin/jw lookup subdomain < user-data.json | $jw/bin/jw unstring > subdomain
    $jw/bin/jw lookup publicKey < user-data.json | $jw/bin/jw unstring | $coreutils/bin/base64 -d > publickey
  '';
  update-dns = builtins.toFile "update-dns.sh" ''
    set -euo pipefail
    gnugrep=$1
    gawk=$2
    curl=$3
    openssl=$4
    netcat=$5

    if [ ! -f /var/jekos/subdomain ]; then
      echo "Missing subdomain." >&2
      exit 1
    fi
    subdomain=$(cat /var/jekos/subdomain)
    ip=$($curl/bin/curl http://169.254.169.254/latest/meta-data/public-ipv4)
    port=80

    api=http://jekos.net/subdomain
    curl="$curl/bin/curl -s -o /dev/null -w %{http_code}"

    mkdir -p /var/jekos.net
    cd /var/jekos.net
    if [ ! -f $subdomain.private ]; then # this is the first time we're claiming this name
      if [ $($curl -I $api/$subdomain) -ne 404 ]; then
        >&2 echo "$subdomain is unavailable."
        exit 2
      fi
      $openssl/bin/openssl genrsa > $subdomain.private
      $openssl/bin/openssl rsa -in $subdomain.private -pubout -out $subdomain.public
    fi

    # trap "kill -- -$$ 2>&1 >/dev/null" SIGINT SIGTERM EXIT
    echo $ip | $openssl/bin/openssl rsautl -sign -inkey $subdomain.private | $netcat/bin/nc -l -p $port -c >/dev/null &

    if [ $($curl -T $subdomain.public -H "Content-Type: application/x-pem-file" $api/$subdomain?ip=$ip\&port=$port) -ne 201 ]; then
      >&2 echo "Failed to register $subdomain."
      exit 3
    fi
    echo -n ".jekos.net" | cat /var/jekos/subdomain - > /var/jekos/domain
  '';
in
{
  kernel = {config, pkgs, resources, ...}:
  {
    deployment = {
      targetEnv = "ec2";
      ec2 = {
        inherit region;
        instanceType = "t2.nano";
        keyPair = resources.ec2KeyPairs.nixops;
      };
    };
    
    systemd.services.fetch-user-data = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''${pkgs.bash}/bin/bash ${fetch-user-data} ${pkgs.coreutils} ${pkgs.curl} ${jw}'';
      };
    };

    systemd.services.update-dns = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "fetch-user-data.service" ];
      after = [ "fetch-user-data.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''${pkgs.bash}/bin/bash ${update-dns} ${pkgs.gnugrep} ${pkgs.gawk} ${pkgs.curl} ${pkgs.openssl} ${pkgs.netcat}'';
      };
    };
  };

  resources.ec2KeyPairs.nixops = { inherit region; };
}
