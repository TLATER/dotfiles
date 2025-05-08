{
  users.users.tlater.extraGroups = [ "networking" ];

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
          # Cloudflare backup
          name = ".";
          forward-addr = "1.1.1.1";
        }
      ];
    };

    localControlSocketPath = "/run/unbound/unbound.ctl";
  };
}
