{ config, lib, pkgs, ... }:

let
  stumpwm = pkgs.lispPackages.stumpwm.overrideAttrs (oldAttrs: rec {
    propagatedBuildInputs = with pkgs; [
      lispPackages.clx-truetype
      lispPackages.xembed
    ] ++ (oldAttrs.propagatedBuildInputs or []);
  });

in stumpwm
