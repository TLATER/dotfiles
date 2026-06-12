/**
  Module to enable everything required to interact with yubikeys.
*/

{ pkgs, ... }: {
  services = {
    udev.packages = [
      # TODO: This is EOL; all we use this for is a single udev rule
      # we could just port over, but it'd be nice if upstream did
      # this.
      pkgs.yubikey-personalization
    ];
    pcscd.enable = true;
  };

  hardware.gpgSmartcards.enable = true;
}
