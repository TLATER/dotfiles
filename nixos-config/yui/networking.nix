{
  networking = {
    hostName = "yui";
    wireless.interfaces = ["wlp6s0"];

    firewall.allowedTCPPorts = [
      # Allow barrier
      24800
      # Allow minecraft for when I'm running a minecraft server
      # locally
      25565
    ];

    firewall.allowedUDPPorts = [
      25565
    ];

    # Work around EAC
    hosts."127.0.0.1" = ["modules-cdn.eac-prod.on.epicgames.com"];
  };

  systemd.network = {
    netdevs = {
      "10-bond0" = {
        netdevConfig = {
          Name = "bond0";
          Kind = "bond";
        };

        bondConfig = {
          Mode = "active-backup";
          PrimaryReselectPolicy = "always";
          MIIMonitorSec = "100ms";
        };
      };
    };

    networks = {
      "10-bond0" = {
        matchConfig.Name = "bond0";
        networkConfig.DHCP = "yes";
      };

      "40-eno1" = {
        matchConfig.Name = "eno1";
        networkConfig = {
          Bond = "bond0";
          PrimarySlave = true;
        };
      };

      "40-wlp6s0" = {
        matchConfig.Name = "wlp6s0";
        networkConfig.Bond = "bond0";
      };
    };
  };
}
