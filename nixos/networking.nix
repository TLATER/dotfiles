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
    networkmanager.enable = true;
    nftables.enable = true;
  };

  # NTP
  services.chrony.enable = true;

  # Timezone
  time.timeZone = lib.mkOverride 99 "Asia/Hong_Kong";
  services.automatic-timezoned.enable = true;
}
