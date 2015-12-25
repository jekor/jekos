let
  region = "us-west-2";
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
    
    systemd.services.fetchkey = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = ''${pkgs.curl}/bin/curl -o /var/publickey http://169.254.169.254/latest/user-data'';
      };
    };

    systemd.services.fetchdomain = {
      wantedBy = [ "multi-user.target" ];
      wants = [ "network-online.target" ];
      after = [ "network-online.target" ];
      serviceConfig = {
        ExecStart = ''${pkgs.curl}/bin/curl -o /var/domain http://169.254.169.254/latest/meta-data/public-hostname'';
      };
    };
  };

  resources.ec2KeyPairs.nixops = { inherit region; };
}
