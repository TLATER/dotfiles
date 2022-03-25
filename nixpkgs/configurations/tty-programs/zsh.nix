{
  pkgs,
  dotroot,
  ...
}: {
  home.packages = with pkgs; [zsh];

  # Basic, local config
  home.file.".zshenv".source = "${dotroot}/dotfiles/zshenv";
  xdg.configFile."zsh" = {
    recursive = true;
    source = "${dotroot}/dotfiles/zsh";
  };

  # Plugins
  xdg.configFile = {
    "zsh/plugins/zsh-background-notify" = {
      recursive = true;
      source = pkgs.local.zsh-background-notify;
    };
    "zsh/oh-my-zsh-expat/plugins/emacs" = {
      recursive = true;
      source = pkgs.local.oh-my-zsh-emacs;
    };
    "zsh/oh-my-zsh-expat/plugins/screen" = {
      recursive = true;
      source = pkgs.local.oh-my-zsh-screen;
    };
    "zsh/oh-my-zsh-expat/tools/" = {
      recursive = true;
      source = pkgs.local.oh-my-zsh-require-tool;
    };
  };
}
