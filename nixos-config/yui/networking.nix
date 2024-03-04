{
  networking = {
    hostName = "yui";

    firewall.allowedTCPPorts = [
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

  networking.networkmanager.ensureProfiles.profiles.bond.bond.primary = "eno1";
}
