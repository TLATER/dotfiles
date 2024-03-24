{
  users.users.tlater.extraGroups = ["networking"];

  networking = {
    useDHCP = false;
    networkmanager.enable = true;
  };

  services.unbound = {
    enable = true;

    settings = {
      server.qname-minimisation = true;

      forward-zone = [
        {
          # ProtonVPN DNS, if available
          name = ".";
          forward-addr = "10.2.0.1";
        }
        {
          # Cloudflare backup
          name = ".";
          forward-addr = "1.1.1.1";
        }
      ];
    };

    localControlSocketPath = "/run/unbound/unbound.ctl";
  };
}
