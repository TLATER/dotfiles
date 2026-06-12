/**
  Set up tailscale.
*/
{
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

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
}
