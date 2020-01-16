{ pkgs, ... }:

let
  local-pkgs = with pkgs; {
    emacs = pkgs.callPackage ./emacs.nix {};
  };
in local-pkgs
