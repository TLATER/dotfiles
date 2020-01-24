{ config, lib, pkgs, ... }:

let local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  # Basic, local config
  home.file.".zshrc".source = ../../dotfiles/zshrc;
  xdg.configFile = {
    "zsh" = {
      recursive = true;
      source = ../../dotfiles/zsh;
    };
  };

  # Plugins
  xdg.configFile = {
    "zsh/plugins/zsh-background-notify" = {
      recursive = true;
      source = local-pkgs.zsh-background-notify;
    };
    "zsh/oh-my-zsh-expat/plugins/emacs" = {
      recursive = true;
      source = local-pkgs.oh-my-zsh-emacs;
    };
    "zsh/oh-my-zsh-expat/plugins/screen" = {
      recursive = true;
      source = local-pkgs.oh-my-zsh-screen;
    };
    "zsh/oh-my-zsh-expat/tools/" = {
      recursive = true;
      source = local-pkgs.oh-my-zsh-require-tool;
    };
  };
}
