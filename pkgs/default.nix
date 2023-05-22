{
  self,
  pkgs,
  flake-inputs,
}: let
  sources = pkgs.callPackage ./sources.nix {};
  callPackage = pkgs.lib.callPackageWith (pkgs // {inherit self sources flake-inputs;});
in {
  # "Packages" that really just contain configuration settings
  firefox-ui-fix = callPackage ./configuration/firefox-ui-fix.nix {};
  oh-my-zsh-emacs = callPackage ./configuration/oh-my-zsh-emacs.nix {};
  oh-my-zsh-screen = callPackage ./configuration/oh-my-zsh-screen.nix {};
  phosphor-icons = callPackage ./configuration/phosphor-icons.nix {};
  tridactyl-emacs = callPackage ./configuration/tridactyl-emacs.nix {};
  zsh-background-notify = callPackage ./configuration/zsh-background-notify.nix {};

  # "Packages" that just contain utility scripts
  commit-nvfetcher = callPackage ./scripts/commit-nvfetcher {};

  # Proper packages
  emacs = callPackage ./applications/emacs {};
  gauth = callPackage ./applications/gauth.nix {};
  stumpwm = callPackage ./applications/stumpwm {};
  stumpwm-contrib = callPackage ./applications/stumpwm/stumpwm-contrib.nix {};
  nextcloudcmd = callPackage ./applications/nextcloudcmd.nix {};
}
