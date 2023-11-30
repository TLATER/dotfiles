{
  config,
  pkgs,
  ...
}: {
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
    eza
    fd
    ripgrep
    screen
  ];

  xdg.configFile."screen/config".source = "${config._dotfiles}/screenrc";
}
