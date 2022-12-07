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
    ./personal
    ./tty-applications
    ./work
    ./xdg-settings.nix
  ];
}
