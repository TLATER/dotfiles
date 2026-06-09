{ pkgs, ... }:
{
  services = {
    udev.packages = [ pkgs.yubikey-personalization ];
    pcscd.enable = true;
  };

  hardware.gpgSmartcards.enable = true;
}
