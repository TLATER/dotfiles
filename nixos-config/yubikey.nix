{ pkgs, ... }:
{
  services = {
    udev.packages = [ pkgs.yubikey-personalization ];
    pcscd.enable = true;
  };

  hardware.gpgSmartcards.enable = true;

  # sops-nix will launch an scdaemon instance on boot, which will stay
  # alive and prevent the yubikey from working with any users that log
  # in later.
  systemd.services.shutdownSopsGpg = {
    path = [ pkgs.gnupg ];
    script = ''
      gpgconf --homedir /var/lib/sops --kill gpg-agent
    '';
    wantedBy = [ "multi-user.target" ];
  };
}
