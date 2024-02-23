{config, ...}: {
  networking.wg-quick.interfaces.wg-tlaternet = {
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

  sops.secrets = {
    "wireguard/tlaternet" = {};
  };
}
