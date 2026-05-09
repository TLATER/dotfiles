{ config, ... }:
{
  networking = {
    nftables.enable = true;

    networkmanager.ensureProfiles = {
      environmentFiles = [ config.sops.secrets.wireless-env.path ];

      profiles = {
        bond = {
          connection = {
            id = "bond";
            type = "bond";
            interface-name = "bond0";
          };

          bond = {
            miimon = 100;
            mode = "active-backup";
            primary_reselect = "always";
            fail_over_mac = "active";
            updelay = 200;
          };

          ipv4.method = "auto";
          ipv6 = {
            addr-gen-mode = "default";
            method = "auto";
          };
        };

        ethernet.connection = {
          id = "ethernet";
          type = "ethernet";

          controller = "bond0";
          port-type = "bond";
        };
      };
    };
  };

  sops.secrets.wireless-env = { };
}
