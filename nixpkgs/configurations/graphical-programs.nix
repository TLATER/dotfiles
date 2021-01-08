{ config, pkgs, ... }:

let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    local-pkgs.cap
  ];
}
