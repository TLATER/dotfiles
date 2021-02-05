{ pkgs, lib, ... }:

{
  imports = [ ./mail/work.nix ];

  home.packages = with pkgs; [ local.gauth ];
  programs.git.userEmail = lib.mkOverride 99 "tristan.maat@codethink.co.uk";
}
