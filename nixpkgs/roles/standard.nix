{ config, lib, pkgs, ... }:

with lib;

let
  helpers = import ../helpers { inherit lib; };
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  options = {
    isWorkProfile = mkOption {
      type = types.bool;
      default = false;
    };

    screenWidth = mkOption {
      type = types.int;
      default = 1920;
    };
  };

  imports = [
    ../configurations/dunst.nix
    ../configurations/emacs.nix
    ../configurations/mail.nix
    ../configurations/stumpwm.nix
    ../configurations/zsh.nix
  ];

  config = {
    home.packages = with pkgs; [
      alacritty
      any-nix-shell
      feh
      llpp
      rofi
      screen
      scrot
      xsel

      # Dev things
      gcc # Required for rustc (mozilla/nixpkgs-mozilla#22)
      rustup

      local-pkgs.pass-rofi
    ];

    home.file = {
      ".env".source = ../../dotfiles/env;
      ".ssh/tlater.pub".source = ../../keys/tlater.pub;
    };

    xdg.configFile = {
      "screen/config".source = ../../dotfiles/screenrc;
    };

    programs = {
      git = {
        enable = true;
        userName = "Tristan Daniël Maat";
        userEmail = if config.isWorkProfile then "tristan.maat@codethink.co.uk" else "tm@tlater.net";
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
          "github.com" = {
            identitiesOnly = true;
            identityFile = "~/.ssh/tlater.pub";
          };
        };
      };
    };

    services = {
      gpg-agent = {
        enable = true;
        enableSshSupport = true;
        defaultCacheTtl = 28800;
      };
    };

    programs.home-manager.enable = true;
    home.stateVersion = "19.09";
  };
}
