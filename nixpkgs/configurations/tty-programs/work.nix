{ pkgs, lib, ... }:

{
  home.packages = with pkgs; [ local.gauth ];
  programs.git.userEmail = lib.mkOverride 99 "tristan.maat@codethink.co.uk";
  programs.gpg.scdaemonSettings.reader-port = "Yubico Yubi";
}
