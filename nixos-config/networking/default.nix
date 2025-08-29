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
          name = ".";
          forward-addr = [
            "9.9.9.9"
            "149.112.112.112"
          ];
        }
      ];
    };

    localControlSocketPath = "/run/unbound/unbound.ctl";
  };
}
