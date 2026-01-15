{
  networking = {
    hostName = "yui";

    # Allow minecraft for when I'm running a minecraft server
    # locally
    firewall = {
      allowedTCPPorts = [ 25565 ];
      allowedUDPPorts = [ 25565 ];
    };

    networkmanager.ensureProfiles.profiles.bond.bond.primary = "eno1";
  };
}
