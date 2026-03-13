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
    };
  };

  sops.secrets.wireless-env = { };
}
