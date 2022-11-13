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
}
