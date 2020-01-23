{ config, lib, pkgs, ... }:


let
  isWorkProfile = false; # TODO: compute from hostname
  helpers = import ./helpers { inherit lib; };
  local-pkgs = import ./local-pkgs { inherit pkgs; };

in {
  home.packages = with pkgs; [
    alacritty
    dunst
    feh
    neomutt
    rofi
    screen
    scrot
    xsel
    zsh

    # Dev things
    python37Packages.ipython
    gcc # Required for rustc (mozilla/nixpkgs-mozilla#22)
    rustup

    # Fonts
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji

    # Custom packages
    local-pkgs.emacs
    local-pkgs.stumpwm
    local-pkgs.stumpwm-contrib
  ];

  home.file = {
    ".emacs.d" = {
      onChange = ''
      # Recompile init files
      SCANNING_PACKAGES=true ${local-pkgs.emacs}/bin/emacs --batch --quick \
            --eval "(byte-recompile-directory user-emacs-directory 0)"

      # Then reload the running emacs config, if any
      ${pkgs.systemd}/bin/systemctl --user reload emacs.service
      '';
      recursive = true;
      source = ../dotfiles/emacs.d;
    };
    ".env" = {
      source = ../dotfiles/env;
    };
    ".mailcap".source = ../dotfiles/mailcap;
    ".Xresources".source = ../dotfiles/Xresources;
    ".zshrc".source = ../dotfiles/zshrc;
    ".ssh/tlater.pub".source = ../keys/tlater.pub;
  };

  xdg.configFile = {
    "dunst/dunstrc".source = ../dotfiles/dunst/dunstrc;
    "fontconfig/fonts.conf".source = ../dotfiles/fonts.conf;
    "neomutt" = {
      recursive = true;
      source = ../dotfiles/neomutt;
    };
    "stumpwm/config" = {
      source = ../dotfiles/stumpwm/config;
      onChange = "${local-pkgs.stumpwm-contrib}/share/stumpwm/modules/util/stumpish/stumpish loadrc";
    };
    "screen/config".source = ../dotfiles/screenrc;
    "zsh" = {
      recursive = true;
      source = ../dotfiles/zsh;
    };
  };

  xsession = {
    enable = true;
    initExtra = ''
    export STUMPWM_CONTRIB_DIR=${local-pkgs.stumpwm-contrib}/share/stumpwm/modules
    export WM=stumpwm
    xrdb -merge ~/.Xresources
    '';
    windowManager.command = "${local-pkgs.stumpwm}/bin/stumpwm-lisp-launcher.sh --eval '(require :asdf)' --eval '(asdf:load-system :stumpwm)' --eval '(stumpwm:stumpwm)'";
  };

  fonts.fontconfig.enable = true;
  programs = {
    git = {
      enable = true;
      userName = "Tristan Daniël Maat";
      userEmail = if isWorkProfile then "tristan.maat@codethink.co.uk" else "tm@tlater.net";
      signing = {
        key = "0x49670FD774E43268";
        signByDefault = true;
      };
    };
    gpg = {
      enable = true;
      settings = {
        fixed-list-mode = true;
        keyid-format = "0xlong";
        personal-digest-preferences = builtins.concatStringsSep " " [
          "SHA512"
          "SHA384"
          "SHA256"
        ];
        personal-cipher-preferences = builtins.concatStringsSep " " [
          "AES256"
          "AES192"
          "AES"
        ];
        default-preference-list = builtins.concatStringsSep " " [
          "SHA512"
          "SHA384"
          "SHA256"
          "AES256"
          "AES192"
          "AES"
          "ZLIB"
          "BZIP2"
          "ZIP"
          "Uncompressed"
        ];
        use-agent = true;
        verify-options = "show-uid-validity";
        list-options = "show-uid-validity";
        cert-digest-algo = "SHA512";
        throw-keyids = true;
        no-emit-version = true;
      };
    };
    mbsync.enable = true;
    msmtp.enable = true;
    password-store = {
      enable = true;
      settings = {
        PASSWORD_STORE_DIR = "${config.xdg.dataHome}/password-store";
        PASSWORD_STORE_KEY = "0xBC7BB2DB17C78E42";
        PASSWORD_STORE_GENERATED_LENGTH = "16";
      };
    };
    ssh = {
      enable = true;
      matchBlocks = {
        "tlater.net" = {
          hostname = "tlater.net";
          user = "tlater";
          port = 2222;
          identitiesOnly = true;
          identityFile = "~/.ssh/tlater.pub";
        };
      };
    };
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
        ExecReload = "${local-pkgs.emacs}/bin/emacsclient --eval \"(load-file user-init-file)\"";
        ExecStart = "${pkgs.runtimeShell} -l -c 'exec ${local-pkgs.emacs}/bin/emacs --fg-daemon'";
        ExecStop = "${local-pkgs.emacs}/bin/emacsclient --eval \"(kill-emacs)\"";
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
          passwordCommand = (helpers.dictToVars config.programs.password-store.settings) + " ${pkgs.pass}/bin/pass codethink.co.uk | ${pkgs.coreutils}/bin/tr -d '\\n'";
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
