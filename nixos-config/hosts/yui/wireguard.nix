{
  pkgs,
  lib,
  config,
  ...
}:
{
  networking.networkmanager = {
    ensureProfiles = {
      environmentFiles = [ config.sops.secrets.wireguard-env.path ];

      profiles.wg-tlaternet = {
        connection = {
          id = "wg-tlaternet";
          type = "wireguard";
          interface-name = "wgt-tlaternet";
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
        source =
          let
            dnsServers =
              (lib.findFirst (zone: zone.name == ".") [ ] config.services.unbound.settings.forward-zone)
              .forward-addr;
          in
          pkgs.writers.writeNu "wireguard-dispatchers.nu" {
            makeWrapperArgs = [
              "--prefix"
              "PATH"
              ":"
              "${lib.makeBinPath [
                pkgs.unbound
              ]}"

              "--set"
              "NIXOS_DNS_SERVERS"
              (lib.concatStringsSep "_" dnsServers)
            ];
          } ./wireguard-dispatchers.nu;
        type = "basic";
      }
    ];
  };

  sops.secrets.wireguard-env = { };
}
