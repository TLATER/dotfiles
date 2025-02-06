{ pkgs, config, ... }:
{
  networking.networkmanager = {
    ensureProfiles = {
      environmentFiles = [ config.sops.secrets.wireguard-env.path ];

      profiles.wg-tlaternet = {
        connection = {
          id = "wg-tlaternet";
          type = "wireguard";
          interface-name = "wgt0";
        };

        ipv4 = {
          address = "10.45.249.2/32";
          method = "manual";
        };

        wireguard = {
          mtu = 1280;
          private-key = "$WG_KEY_TLATERNET";
        };

        "wireguard-peer.73z3Pga/2BCxETYM/qCT2FM1JUCUvQ+Cp+8ROxjhu0w=" = {
          endpoint = "116.202.158.55:51820";
          allowed-ips = "10.45.249.0/24";
          persistentKeepalive = 25;
        };
      };
    };

    dispatcherScripts = [
      {
        source = pkgs.writeShellScript "wgDomainHook" ''
          if [ "$1" == "wgt0" ] && [ "$2" == "up" ]; then
              ${pkgs.unbound}/bin/unbound-control local_zone tlater.net. redirect
              ${pkgs.unbound}/bin/unbound-control local_data tlater.net. A 10.45.249.1
          fi

          if [ "$1" == "wgt0" ] && [ "$2" == "down" ]; then
              ${pkgs.unbound}/bin/unbound-control local_zone_remove tlater.net.
          fi
        '';
        type = "basic";
      }
    ];
  };

  sops.secrets.wireguard-env = { };
}
