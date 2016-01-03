{ tld ? "com" }:
let
  # To deal with an unusable (for me) release and master, I'm editing
  # a checkout of nixpkgs.
  pkgs = import ~/code/nixpkgs { system = "x86_64-linux"; config.allowUnfree = true; };
  tpkgs = pkgs;
  hpkgs = pkgs.haskell.packages.ghc7102;
  jekosKernel = hpkgs.callPackage ../kernel { };
  ui = hpkgs.callPackage ../ui { uglify = pkgs.nodePackages.uglify-js;
                                 stylus = pkgs.nodePackages.stylus; };
  fetch-cert = builtins.toFile "fetch-cert.sh" ''
    set -euo pipefail
    openssl=$1
    python2=$2
    simp_le=$3

    if [ ! -f /var/jekos/domain ]; then
      echo "Missing domain." >&2
      exit 1
    fi
    domain=$(cat /var/jekos/domain)
    echo "domain: $domain" >&2
    mkdir -p /var/letsencrypt
    if [ ! -f /var/letsencrypt/$domain/cert.pem ]; then
      echo "No certificate found. Requesting one."
      mkdir -p /var/letsencrypt/$domain/www
      cd /var/letsencrypt/$domain/www && $python2/bin/python -m SimpleHTTPServer 80 &
      pid=$!
      cd /var/letsencrypt/$domain
      # $simp_le/bin/simp_le --server https://acme-v01.api.letsencrypt.org/directory -d $domain:/var/letsencrypt/$domain/www -f cert.pem -f fullchain.pem -f key.pem
      $simp_le/bin/simp_le --server https://acme-staging.api.letsencrypt.org/directory -d $domain:/var/letsencrypt/$domain/www -f cert.pem -f fullchain.pem -f key.pem
      kill $pid
    else
      # TODO: Check that the domain matches the cert for cases when
      # the domain has changed (e.g. an EC2 instance was stopped and
      # then started.
      if ! $openssl/bin/openssl x509 -in /var/letsencrypt/$domain/cert.pem -checkend 86400 -noout; then
        echo "Certificate expired. Trying to renew."
      fi
    fi
  '';
in
{
  network.description = "jekos";

  kernel = {config, pkgs, ...}:
  {
    imports = [
      ./common.nix
    ];

    environment.systemPackages = [
    ];

    networking.useNetworkd = true;
    networking.interfaces.lo.ip4 = [
      { address = "127.0.0.1"; prefixLength = 8; }
    ];
    networking.firewall.allowedTCPPorts = [ 22 80 443 ];

    systemd.services.fetch-cert = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "update-dns.service" ];
      after = [ "update-dns.service" ];
      serviceConfig = {
        Type = "oneshot";
        ExecStart = ''${tpkgs.bash}/bin/bash ${fetch-cert} ${tpkgs.openssl} ${tpkgs.python2} ${tpkgs.simp_le}'';
      };
    };

    systemd.services.jekos-kernel = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "fetch-cert.service" ];
      after = [ "fetch-cert.service" ];
      serviceConfig = {
        ExecStart = ''${jekosKernel}/bin/kernel ${ui}'';
      };
    };
  };
}
