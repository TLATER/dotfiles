{ pkgs, ... }:

{
  home.packages = with pkgs; [ multimc jre8 ];
}
