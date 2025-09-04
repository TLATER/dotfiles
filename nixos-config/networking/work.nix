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

      tgr = {
        connection = {
          id = "tgr";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 100;
        };

        wifi = {
          mode = "infrastructure";
          ssid = "The Great Room";
          # Disable if we get issues after the transition on 21 September
          # scan-rand-mac-address = false;
        };

        wifi-security = {
          key-mgmt = "wpa-psk";
          psk = "$PSK_TGR";
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
