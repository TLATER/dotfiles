{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (lib.strings) concatMapStringsSep;
  tlaterpkgs = flake-inputs.self.packages.${pkgs.system};
in {
  home.packages = with pkgs; [
    any-nix-shell
  ];

  programs = {
    direnv = {
      enable = true;
      enableZshIntegration = true;
      nix-direnv.enable = true;
    };

    zsh = {
      enable = true;
      dotDir = ".config/zsh";
      history.path = "${config.xdg.dataHome}/zsh_history";

      enableAutosuggestions = true;
      enableCompletion = true;
      enableSyntaxHighlighting = true;
      enableVteIntegration = true;
      defaultKeymap = "emacs";

      plugins = [
        {
          name = "bgnotify";
          src = tlaterpkgs.zsh-background-notify;
        }
        {
          name = "emacs";
          src = tlaterpkgs.oh-my-zsh-emacs;
        }
        {
          name = "screen";
          src = tlaterpkgs.oh-my-zsh-screen;
        }
      ];

      shellAliases = {
        "ls" = "exa";
        "winetricks" = "winetricks -q";
        "pbcopy" = "xsel --clipboard --input";
        "pbpaste" = "xsel --clipboard";
      };

      initExtraFirst = ''
        if [[ -o interactive ]] && \
           (( $+commands[screen] )) && \
           [ -f "$XDG_CONFIG_HOME/screen/config" ] && \
           [ -z "$STY"  ] &&; then
            exec screen -AxRR -c "$XDG_CONFIG_HOME/screen/config"
        fi
      '';

      # Remove `/` from the word chars so we can jump through paths
      # neatly
      completionInit = ''WORDCHARS="''${WORDCHARS/\//}"'';

      initExtra =
        ''
          # Disable C-s freezing the terminal
          stty -ixon

          # Source any-nix-shell
          if (( $+commands[any-nix-shell] )); then
              any-nix-shell zsh | source /dev/stdin
          fi
        ''
        + concatMapStringsSep "\n" builtins.readFile [
          "${config._dotfiles}/zsh/theme.zsh"
          "${config._dotfiles}/zsh/functions.zsh"
        ];
    };
  };
}
