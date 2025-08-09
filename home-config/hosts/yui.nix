{ flake-inputs, pkgs, ... }:
{
  imports = [
    ../config
    ../config/applications
    ../config/desktop
    ../config/services
    ../config/shell
    ../config/xdg-settings.nix

    ../config/personal
  ];

  home.packages = [ flake-inputs.self.packages.${pkgs.system}.jazz-jackrabbit-2 ];
}
