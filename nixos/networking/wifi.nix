/**
  Configuration for wifi networks.
*/
{
  networking.networkmanager.ensureProfiles = {
    environmentFiles = [ "/run/credentials/NetworkManager-ensure-profiles.service/wifi-passwords" ];

    profiles = {
      maatjies = {
        connection = {
          id = "maatjies";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 100;
        };

        wifi = {
          mode = "infrastructure";
          ssid = "maatjies";
        };

        wifi-security = {
          key-mgmt = "wpa-psk";
          psk = "$PSK_MAATJIES";
        };
      };

      lala = {
        connection = {
          id = "lala";
          type = "wifi";

          autoconnect = true;
          autoconnect-priority = 100;
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
    };
  };

  systemd.services.NetworkManager-ensure-profiles.serviceConfig.LoadCredentialEncrypted = [
    "wifi-passwords"
  ];
}
