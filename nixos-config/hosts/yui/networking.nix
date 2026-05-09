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

  services.tailscale = {
    enable = true;
    openFirewall = true;
    useRoutingFeatures = "client";

    extraUpFlags = [ "--login-server=https://tailscale.tlater.net" ];
    extraSetFlags = [
      "--operator=tlater"
      "--webclient"
    ];
  };

  networking = {
    nftables.enable = true;

    firewall = {
      enable = true;
      trustedInterfaces = [ "tailscale0" ];
    };
  };
}
