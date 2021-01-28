{ config, lib, pkgs, ... }:

let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    any-nix-shell
    pass
    screen
  ] ++ (if config.isWorkProfile then [ local-pkgs.gauth ] else []);

  home.file = {
    ".profile".source = ../../dotfiles/env;
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
      extraConfig = {
        github.user = "tlater";
        gitlab.user = "tlater";
        "gitlab.gitlab.codethink.co.uk/api/v4" = {
          user = "tristanmaat";
        };
        url = {
          "ssh://git@".pushInsteadOf = "https://";
        };
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
        throw-keyids = false;
        no-emit-version = true;
      };
    };
    ssh = {
      enable = true;
      matchBlocks = {
        "*" = {
          identitiesOnly = true;
          identityFile = "~/.ssh/tlater.pub";
        };
        "tlater.net" = lib.hm.dag.entryAfter ["*"] {
          hostname = "tlater.net";
          user = "tlater";
          port = 2222;
        };
      };
    };
  };

  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtl = 86400;
      maxCacheTtl = 2592000;
    };
  };
}
