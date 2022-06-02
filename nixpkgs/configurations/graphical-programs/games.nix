{
  pkgs,
  lib,
  ...
}: let
  inherit (lib.lists) reverseList;
  inherit (lib.strings) concatStringsSep;
in {home.packages = with pkgs; [polymc];}
