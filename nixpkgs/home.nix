{ pkgs, ... }:

{
  home.packages = with pkgs; [
    alacritty
    dunst
    screen

    (import ./emacs.nix { inherit pkgs; })
  ];

  programs.home-manager.enable = true;
  home.stateVersion = "19.09";
}
