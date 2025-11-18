{ config, ... }:
{
  networking = {
    nftables.enable = true;

    networkmanager = {
      ensureProfiles.environmentFiles = [ config.sops.secrets.wireless-env.path ];

      ensureProfiles.profiles = {
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

        ethernet = {
          connection = {
            id = "ethernet";
            type = "ethernet";

            master = "bond0";
            slave-type = "bond";
          };
        };

        lala = {
          connection = {
            id = "lala";
            type = "wifi";

            master = "bond0";
            slave-type = "bond";

            autoconnect = true;
            autoconnect-priority = 101;
          };

          wifi = {
            mode = "infrastructure";
            ssid = "lala";
          };

          wifi-security = {
            key-mgmt = "sae";
            psk = "$PSK_LALA";
          };
        };

        mikan = {
          connection = {
            id = "mikan";
            type = "wifi";

            master = "bond0";
            slave-type = "bond";

            autoconnect = true;
            autoconnect-priority = 100;
          };

          wifi = {
            mode = "infrastructure";
            ssid = "mikan";
          };

          wifi-security = {
            key-mgmt = "sae";
            psk = "$PSK_MIKAN";
          };
        };
      };
    };
  };

  sops.secrets.wireless-env = { };
}
