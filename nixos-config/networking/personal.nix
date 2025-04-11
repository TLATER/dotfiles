{
  lib,
  pkgs,
  config,
  ...
}:
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
            key-mgmt = "wpa-psk";
            psk = "$PSK_MIKAN";
          };
        };
      };

      dispatcherScripts =
        let
          nmcli = lib.getExe' pkgs.networkmanager "nmcli";
          unbound-control = lib.getExe' pkgs.unbound-with-systemd "unbound-control";
        in
        [
          {
            source = pkgs.writeShellScript "mikanDNSHook" ''
              if [ "$2" == "dns-change" ] || [ "$2" == "dhcp4-change" ] || [ "$2" == "dhcp6-change" ]; then
                  mikan_p=$(${nmcli} -g GENERAL.STATE c s mikan | grep -qE '\bactiv')
                  nameservers="$(grep 'nameserver' /run/NetworkManager/resolv.conf | cut -d ' ' -f 2)"

                  # Trust my local intranet's DNS servers
                  if $mikan_p; then
                      # Deliberate word splitting
                      logger "Switching to DNS servers: " $nameservers
                      ${unbound-control} forward $nameservers
                  else
                      logger "Resetting to default unbound configuration"
                      ${unbound-control} reload
                  fi
              fi
            '';
            type = "basic";
          }
        ];
    };
  };

  sops.secrets.wireless-env = { };
}
