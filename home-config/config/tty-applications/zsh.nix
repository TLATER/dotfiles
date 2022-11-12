{
  config,
  pkgs,
  flake-inputs,
  ...
}: let
  tlaterpkgs = flake-inputs.self.packages.${pkgs.system};
in {
  home.packages = with pkgs; [zsh];

  # Basic, local config
  home.file.".zshenv".source = "${config._dotfiles}/dotfiles/zshenv";
  xdg.configFile."zsh" = {
    source = "${config._dotfiles}/dotfiles/zsh";
  };

  programs.nix-index.enable = true;

  # Plugins
  xdg.configFile = {
    "zshplugins/zsh-background-notify" = {
      source = tlaterpkgs.zsh-background-notify;
    };
    "zshplugins/oh-my-zsh-expat/plugins/emacs" = {
      source = tlaterpkgs.oh-my-zsh-emacs;
    };
    "zshplugins/oh-my-zsh-expat/plugins/screen" = {
      source = tlaterpkgs.oh-my-zsh-screen;
    };
    "zshplugins/oh-my-zsh-expat/tools/" = {
      source = tlaterpkgs.oh-my-zsh-require-tool;
    };
  };
}
