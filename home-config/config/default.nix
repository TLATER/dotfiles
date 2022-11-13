{
  config,
  lib,
  ...
}: {
  imports = [
    ./options.nix
    ./non-nixos.nix

    ./desktop
    ./graphical-applications
    ./tty-applications
    ./work
    ./xdg-settings.nix
  ];
}
