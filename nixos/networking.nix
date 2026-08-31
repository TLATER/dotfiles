/**
  Configuration related to networking.
*/
{ lib, ... }: {
  imports = [
    ./networking/dns.nix
    ./networking/vm-bridge.nix
    ./networking/wifi.nix
  ];

  # TODO: Assert this is actually necessary
  users.users.tlater.extraGroups = [ "networking" ];

  services.unbound.enable = true;

  networking = {
    # Not needed since NetworkManager uses its own internal dhcp
    # client.
    dhcpcd.enable = false;
    nftables.enable = true;

    networkmanager = {
      enable = true;

      # We need at least an ethernet profile to get NetworkManager to
      # attempt an ethernet connection.
      ensureProfiles.profiles.ethernet.connection = {
        id = "ethernet";
        type = "ethernet";
      };
    };
  };

  # NTP
  services.chrony.enable = true;

  # Timezone
  time.timeZone = lib.mkOverride 99 "Asia/Hong_Kong";
  services.automatic-timezoned.enable = true;
}
