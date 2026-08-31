/**
  Configuration for docker.
*/
{ config, lib, ... }: {
  virtualisation.docker = {
    enable = true;
    enableOnBoot = false;

    autoPrune.enable = true;

    daemon.settings = {
      live-restore = false;
    };
  };

  networking = {
    nftables.enable = lib.mkForce false;

    # Allow docker containers to communicate
    firewall.extraCommands =
      let
        # Either get the docker daemon setting *or* the default value
        dockerAddressPools =
          config.virtualisation.docker.daemon.settings.default-address-pools or [
            {
              base = "172.30.0.0/16";
              size = 24;
            }
            {
              base = "172.31.0.0/16";
              size = 24;
            }
          ];
        addresses = lib.concatMapStringsSep "," (pool: pool.base) dockerAddressPools;
      in
      ''
        iptables -A INPUT -s ${addresses} -d ${addresses},172.17.0.1 -j ACCEPT
      '';
  };

  users.users.tlater.extraGroups = [ "docker" ];
}
