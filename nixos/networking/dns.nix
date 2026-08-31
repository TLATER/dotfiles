/**
  DNS configuration.
*/
{ config, lib, ... }:
lib.mkMerge [
  # If tailscale is used, we just leave everything with the default
  # config - shortly after connection to a network, tailscale should
  # take over DNS, and we are happy.
  {
    services.resolved = {
      enable = true;
      settings.Resolve.DNSSEC = true;
    };
  }

  # If tailscale is *not* used, we attempt to override the DNS servers
  # as much as possible.
  (lib.mkIf (!config.services.tailscale.enable) {
    services.resolved.settings.Resolve = {
      Domains = "~.";
      DNS = [
        "9.9.9.9#dns.quad9.net"
        "149.112.112.112#dns.quad9.net"
        "2620:fe::fe#dns.quad9.net"
        "2620:fe::9#dns.quad9.net"
      ];

      DNSOverTLS = true;
    };
  })
]
