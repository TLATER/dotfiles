{ flake-inputs, pkgs, ... }:
{
  imports = [
    ../.
    ../applications
    ../desktop
    ../services
    ../shell
    ../xdg-settings.nix

    ../personal
  ];

  home.packages = [ flake-inputs.self.packages.${pkgs.system}.jazz-jackrabbit-2 ];
}
