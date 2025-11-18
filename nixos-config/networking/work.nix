{ config, lib, ... }:
{
  networking.networkmanager.ensureProfiles = lib.mkForce {
    environmentFiles = [ config.sops.secrets.wireless-env.path ];

    profiles = {
      lala-guest = {
        connection = {
          id = "lala-guest";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 101;
        };

        wifi = {
          mode = "infrastructure";
          ssid = "lala-guest";
        };

        wifi-security = {
          key-mgmt = "sae";
          psk = "$PSK_LALA_GUEST";
        };
      };

      mikan-guest = {
        connection = {
          id = "mikan-guest";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 100;
          # Attempt to connect indefinitely
          autoconnect-retries = 0;
        };

        wifi = {
          mode = "infrastructure";
          ssid = "mikan-guest";
        };

        wifi-security = {
          key-mgmt = "sae";
          psk = "$PSK_MIKAN_GUEST";
        };

        ipv4.method = "auto";
        ipv6 = {
          addr-gen-mode = "default";
          method = "auto";
        };
      };
    };
  };

  sops.secrets.wireless-env = { };
}
