{ config, lib, pkgs, ... }:

with import ./helpers.nix { inherit lib; };

let
  isWorkProfile = false; # TODO: compute from hostname
  custom-emacs = import ./emacs.nix { inherit pkgs; };

in {
  home.packages = with pkgs; [
    alacritty
    dunst
    neomutt
    python37Packages.ipython
    screen
    zsh

    # Fonts
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji

    # Custom packages
    custom-emacs
  ];

  home.file = {
    ".emacs.d" = {
      onChange = "${pkgs.systemd}/bin/systemctl --user reload emacs.service";
      recursive = true;
      source = ../dotfiles/emacs.d;
    };
    ".env" = {
      source = ../dotfiles/env;
    };
    ".mailcap".source = ../dotfiles/mailcap;
    ".Xresources".source = ../dotfiles/Xresources;
    ".zshrc".source = ../dotfiles/zshrc;
  };

  xdg.configFile = {
    "dunst/dunstrc".source = ../dotfiles/dunst/dunstrc;
    "fontconfig/fonts.conf".source = ../dotfiles/fonts.conf;
    "neomutt" = {
      recursive = true;
      source = ../dotfiles/neomutt;
    };
    "zsh" = {
      recursive = true;
      source = ../dotfiles/zsh;
    };
  };

  fonts.fontconfig.enable = true;
  programs = {
    feh.enable = true;
    git = {
      enable = true;
      userName = "Tristan Daniël Maat";
      userEmail = if isWorkProfile then "tristan.maat@codethink.co.uk" else "tm@tlater.net";
      signing = {
        key = if isWorkProfile then "B32554B9C8BA03ED" else "4CFAC122454FB978";
        signByDefault = true;
      };
    };
    mbsync.enable = true;
    msmtp.enable = true;
    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_KEY = "0x9FAF1AA48509A7F1";
        PASSWORD_STORE_GENERATED_LENGTH = "16";
      };
    };
    # Rofi is configured in .Xresources
    rofi.enable = true;
  };

  services = {
    # Dunst is configured in ~/.config/dunst/dunstrc
    dunst.enable = true;
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 28800;
    };
    mbsync.enable = true;
  };

  systemd.user.services = {
    emacs = {
      Unit = {
        Description = "Emacs: the extensible, self-documenting text editor";
        Documentation = "info:emacs man:emacs(1) https://gnu.org/software/emacs";
        X-RestartIfChanged = false;
      };
      Service = {
        ExecReload = "${custom-emacs}/bin/emacsclient --eval \"(load-file user-init-file)\"";
        ExecStart = "${pkgs.runtimeShell} -l -c 'exec ${custom-emacs}/bin/emacs --fg-daemon'";
        ExecStop = "${custom-emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
        Restart = "on-failure";
      };
      Install = {
        WantedBy = ["default.target"];
      };
    };
  };

  accounts = {
    email = {
      accounts = {
        "codethink.co.uk" = {
          address = "tristan.maat@codethink.co.uk";
          primary = isWorkProfile;
          realName = "Tristan Daniël Maat";

          userName = "tristanmaat";
          passwordCommand = (dictToVars config.programs.password-store.settings) + " ${pkgs.pass}/bin/pass codethink.co.uk | ${pkgs.coreutils}/bin/tr -d '\\n'";
          imap = {
            host = "mail.codethink.co.uk";
            port = 993;
          };
          smtp = {
            host = "mail.codethink.co.uk";
            port = 465;
          };

          mbsync = {
            create = "maildir";
            enable = isWorkProfile;
          };
          msmtp = {
            enable = isWorkProfile;
            extraConfig = {
              from = "tristan.maat@codethink.co.uk";
            };
          };
        };
      };
      maildirBasePath = "${config.xdg.dataHome}/mail";
    };
  };

  programs.home-manager.enable = true;
  home.stateVersion = "19.09";
}
