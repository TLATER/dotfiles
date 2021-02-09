{ pkgs, ... }:

{
  home.packages = with pkgs; [ unstable.multimc jre8 ];
}
