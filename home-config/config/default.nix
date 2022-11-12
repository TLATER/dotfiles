{
  config,
  lib,
  ...
}: {
  imports = [
    ./options.nix

    ./desktop
    ./graphical-applications
    ./tty-applications
    ./work
    ./xdg-settings.nix
  ];

  config = lib.mkIf config.custom.has-yubikey {
    programs.ssh.matchBlocks."*" = {
      identitiesOnly = true;
      identityFile = "~/.ssh/tlater.pub";
    };
  };
}
