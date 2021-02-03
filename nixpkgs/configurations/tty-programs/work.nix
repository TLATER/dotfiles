{ pkgs, ... }:

{
  imports = [ ./mail/work.nix ];

  home.packages = with pkgs; [ gauth ];
  programs.git.userEmail = "tristan.maat@codethink.co.uk";
}
