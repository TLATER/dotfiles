{ pkgs }:

let
  sources = pkgs.callPackage ./sources.nix { };
  callPackage = pkgs.lib.callPackageWith (pkgs // { inherit sources; });
in {
  background = callPackage ./background.nix { };
  cap = callPackage ./cap.nix { };
  commit-nvfetcher = callPackage ./commit-nvfetcher.nix { };
  dump-ics = callPackage ./dump-ics.nix { };
  emacs = callPackage ./emacs.nix { };
  firefox-ui-fix = callPackage ./firefox-ui-fix.nix { };
  gauth = callPackage ./gauth.nix { };
  gcs = callPackage ./gcs.nix { };
  oh-my-zsh-emacs = callPackage ./oh-my-zsh-emacs.nix { };
  oh-my-zsh-require-tool = callPackage ./oh-my-zsh-require-tool.nix { };
  oh-my-zsh-screen = callPackage ./oh-my-zsh-screen.nix { };
  pass-rofi = callPackage ./pass-rofi.nix { };
  read-sops = callPackage ./read-sops.nix { };
  stumpwm = callPackage ./stumpwm.nix { };
  stumpwm-contrib = callPackage ./stumpwm-contrib.nix { };
  tridactyl-emacs = callPackage ./tridactyl-emacs.nix { };
  zsh-background-notify = callPackage ./zsh-background-notify.nix { };
}
