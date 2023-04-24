{
  config,
  lib,
  ...
}: {
  networking.wg-quick.interfaces = {
    wg-pvpn-hk-6 = {
      autostart = false;

      privateKeyFile = config.sops.secrets."protonvpn/hk-6".path;
      address = ["10.2.0.2/32"];
      dns = ["10.2.0.1"];

      peers = [
        {
          publicKey = "nkXNip76ZowVBdPQMeQUe6uLXRFFHVbdhn3n+CaI8j4=";
          endpoint = "193.239.86.2:51820";
          allowedIPs = ["0.0.0.0/0" "::/0"];
        }
      ];
    };

    wg-pvpn-hk-21 = {
      autostart = false;

      privateKeyFile = config.sops.secrets."protonvpn/hk-21".path;
      address = ["10.2.0.2/32"];
      dns = ["10.2.0.1"];

      peers = [
        {
          publicKey = "b04WYLiUOie4OkYbneVXdqnmoGKZyU7Vpfb9N+Qf31c=";
          endpoint = "156.146.45.129:51820";
          allowedIPs = ["0.0.0.0/0" "::/0"];
        }
      ];
    };

    wg-pvpn-hk-41 = {
      autostart = false;

      privateKeyFile = config.sops.secrets."protonvpn/hk-41".path;
      address = ["10.2.0.2/32"];
      dns = ["10.2.0.1"];

      peers = [
        {
          publicKey = "/AEriTfHYyrhW+bj1cDy9RroL4j4o1tv9sw4m+aB8lA=";
          endpoint = "146.70.113.98:51820";
          allowedIPs = ["0.0.0.0/0" "::/0"];
        }
      ];
    };

    wg-tlaternet = {
      autostart = true;

      privateKeyFile = config.sops.secrets."wireguard/tlaternet".path;
      address = ["10.45.249.2/32"];
      # This is the minimum MTU possible (for IPv6 traffic), so should
      # be the most generally supported.
      #
      # Better connections should support more, but my home connection
      # only seems to support 1310 anyway, and this computer will
      # likely travel, so for set-it-and-forget-it reasons let's set
      # it to the minimum.
      mtu = 1280;

      peers = [
        {
          publicKey = "73z3Pga/2BCxETYM/qCT2FM1JUCUvQ+Cp+8ROxjhu0w=";
          endpoint = "178.79.137.55:51820";
          allowedIPs = ["10.45.249.0/24"];
          persistentKeepalive = 25;
        }
      ];
    };
  };

  sops.secrets = {
    "protonvpn/hk-6" = {};
    "protonvpn/hk-21" = {};
    "protonvpn/hk-41" = {};
    "wireguard/tlaternet" = {};
  };
}
