{
  users.users.tlater.extraGroups = [ "networking" ];

  networking = {
    useDHCP = false;
    networkmanager.enable = true;
  };

  services.unbound = {
    enable = true;

    settings = {
      server = {
        qname-minimisation = true;
        tls-system-cert = true;
      };

      forward-zone = [
        {
          name = ".";
          forward-tls-upstream = true;
          forward-addr = [
            "9.9.9.9@853#dns.quad9.net"
            "149.112.112.112@853#dns.quad9.net"
            "2620:fe::fe@853#dns.quad9.net"
            "2620:fe::9@853#dns.quad9.net"
          ];
        }
      ];
    };

    localControlSocketPath = "/run/unbound/unbound.ctl";
  };
}
