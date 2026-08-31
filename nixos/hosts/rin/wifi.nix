/**
  Wifi networks only used on rin.
*/
{
  systemd.services.NetworkManager-ensure-profiles.serviceConfig.LoadCredentialEncrypted = [
    "work-wifi-passwords"
  ];

  networking.networkmanager.ensureProfiles = {
    environmentFiles = [
      "/run/credentials/NetworkManager-ensure-profiles.service/work-wifi-passwords"
    ];

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
}
