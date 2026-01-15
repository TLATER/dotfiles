{ lib, pkgs, ... }:
{
  users.users.tlater.extraGroups = [ "networking" ];

  environment.systemPackages = [ pkgs.dig ];

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
          forward-addr = lib.flatten (
            lib.mapAttrsToList (domain: map (ip: "${ip}@853#${domain}")) {
              "dns.quad9.net" = [
                "9.9.9.9"
                "149.112.112.112"
                "2620:fe::fe"
                "2620:fe::9"
              ];

              "one.one.one.one" = [
                "1.1.1.1"
                "1.0.0.1"
                "2606:4700:4700::1111"
                "2606:4700:4700::1001"
              ];
            }
          );
        }
      ];
    };

    localControlSocketPath = "/run/unbound/unbound.ctl";
  };

  # Ensure unbound is available for DNS settings by the time
  # connections might set such
  systemd.services.unbound.after = lib.mkForce [ ];
  systemd.services.unbound.before = [ "NetworkManager.service" ];
}
