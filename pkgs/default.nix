{
  self,
  pkgs,
}: let
  sources = pkgs.callPackage ./sources.nix {};
  callPackage = pkgs.lib.callPackageWith (pkgs // {inherit self sources;});
in {
  background = callPackage ./background.nix {};
  cap = callPackage ./cap.nix {};
  commit-nvfetcher = callPackage ./commit-nvfetcher.nix {};
  dump-ics = callPackage ./dump-ics.nix {};
  emacs = callPackage ./emacs.nix {};
  firefox-ui-fix = callPackage ./firefox-ui-fix.nix {};
  gauth = callPackage ./gauth.nix {};
  oh-my-zsh-emacs = callPackage ./oh-my-zsh-emacs.nix {};
  oh-my-zsh-screen = callPackage ./oh-my-zsh-screen.nix {};
  pass-rofi = callPackage ./pass-rofi.nix {};
  setup-wacom = callPackage ./setup-wacom.nix {};
  stumpwm = callPackage ./stumpwm {};
  stumpwm-contrib = callPackage ./stumpwm/stumpwm-contrib.nix {};
  tridactyl-emacs = callPackage ./tridactyl-emacs.nix {};
  zsh-background-notify = callPackage ./zsh-background-notify.nix {};
}
