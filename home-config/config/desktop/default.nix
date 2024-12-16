{ pkgs, ... }:
{
  imports = [
    ./dunst.nix
    ./gtk.nix
    ./mime.nix
    ./rofi.nix
    ./sway.nix
  ];

  home.packages = with pkgs; [
    caffeine-ng
  ];
  services.caffeine.enable = true;
  xsession.importedVariables = [ "PATH" ];
}
