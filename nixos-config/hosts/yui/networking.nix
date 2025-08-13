{
  networking = {
    hostName = "yui";

    firewall.allowedTCPPorts = [
      # Allow minecraft for when I'm running a minecraft server
      # locally
      25565
    ];

    firewall.allowedUDPPorts = [ 25565 ];
  };

  networking.networkmanager.ensureProfiles.profiles.bond.bond.primary = "eno1";
}
