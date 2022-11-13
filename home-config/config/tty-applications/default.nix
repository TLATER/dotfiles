{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (flake-inputs.self.packages.${pkgs.system}) system-update;
in {
  imports = [
    ./aria.nix
    ./emacs.nix
    ./git.nix
    ./gpg.nix
    ./ssh.nix
    ./zsh.nix
  ];

  home.packages = with pkgs; [
    bat
    exa
    fd
    ripgrep
    screen

    system-update
  ];

  home.file.".profile".source = "${config._dotfiles}/env";
  xdg.configFile."screen/config".source = "${config._dotfiles}/screenrc";
}
