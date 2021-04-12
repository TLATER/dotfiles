{ pkgs }:

with pkgs;

{
  background = pkgs.callPackage ./background.nix { };
  cap = pkgs.callPackage ./cap.nix { };
  dump-ics = pkgs.callPackage ./dump-ics.nix { };
  # FIXME: When 21.5 is stable, switch back to stable pkgs
  emacs = pkgs.unstable.callPackage ./emacs.nix { };
  gauth = pkgs.callPackage ./gauth.nix { };
  gcs = pkgs.unstable.callPackage ./gcs.nix { };
  oh-my-zsh-emacs = pkgs.callPackage ./oh-my-zsh-emacs.nix { };
  oh-my-zsh-require-tool = pkgs.callPackage ./oh-my-zsh-require-tool.nix { };
  oh-my-zsh-screen = pkgs.callPackage ./oh-my-zsh-screen.nix { };
  pass-rofi = pkgs.callPackage ./pass-rofi.nix { };
  read-sops = pkgs.callPackage ./read-sops.nix { };
  stumpwm = pkgs.callPackage ./stumpwm.nix { };
  stumpwm-contrib = pkgs.callPackage ./stumpwm-contrib.nix { };
  zsh-background-notify = pkgs.callPackage ./zsh-background-notify.nix { };
}
