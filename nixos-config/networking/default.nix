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
        # Sends less identifying data for queries
        qname-minimisation = true;

        # Prevent DNSSEC algorithm downgrades
        harden-algo-downgrade = true;

        # These addresses are forbidden from public responses, but
        # unbound does not enforce this by default
        private-address = [
          "10.0.0.0/8"
          "172.16.0.0/12" # Maybe allow these because ad blockers use them
          "192.168.0.0/16"
          "169.254.0.0/16"
          "fd00::/8"
          "fe80::/10"
          "::ffff:0:0/96"
        ];
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
