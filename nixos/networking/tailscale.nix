/**
  Set up tailscale.
*/
{ lib, ... }: {
  networking.firewall.trustedInterfaces = [ "tailscale0" ];

  # Systemd `.target` units automatically set an `After=` relation for
  # services that want them.
  #
  # In this case, that means adding an implicit dependency on
  # `NetworkManager-wait-online.service` to *everything*.
  #
  # This is stupid, so we make sure that the tailscale units actually
  # start *after* `multi-user.target`.
  systemd.services =
    lib.pipe
      [ "tailscaled" "tailscaled-set" ]
      [
        (map (name: lib.nameValuePair name { unitConfig.After = [ "multi-user.target" ]; }))
        lib.listToAttrs
      ];

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
