{ pkgs, ... }:

let
  local-pkgs = with pkgs; {
    background = pkgs.callPackage ./background.nix {};
    emacs = pkgs.callPackage ./emacs.nix {};
    stumpwm = pkgs.callPackage ./stumpwm.nix {};
    stumpwm-contrib = pkgs.callPackage ./stumpwm-contrib.nix {};
  };
in local-pkgs
