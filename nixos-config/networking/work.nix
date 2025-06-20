{ config, lib, ... }:
{
  networking.networkmanager.ensureProfiles = lib.mkForce {
    environmentFiles = [ config.sops.secrets.wireless-env.path ];

    profiles = {
      mikan-guest = {
        connection = {
          id = "mikan-guest";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 100;
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
