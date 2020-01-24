{ pkgs }:

with pkgs;

{
  background = pkgs.callPackage ./background.nix {};
  emacs = pkgs.callPackage ./emacs.nix {};
  oh-my-zsh-emacs = pkgs.callPackage ./oh-my-zsh-emacs.nix {};
  oh-my-zsh-require-tool = pkgs.callPackage ./oh-my-zsh-require-tool.zsh {};
  oh-my-zsh-screen = pkgs.callPackage ./oh-my-zsh-screen.nix {};
  pass-rofi = pkgs.callPackage ./pass-rofi.nix {};
  stumpwm = pkgs.callPackage ./stumpwm.nix {};
  stumpwm-contrib = pkgs.callPackage ./stumpwm-contrib.nix {};
  zsh-background-notify = pkgs.callPackage ./zsh-background-notify.nix {};
}
