{ config, lib, pkgs, dotroot, ... }:

{
  imports = [ ./emacs.nix ./zsh.nix ];

  home.packages = with pkgs; [
    any-nix-shell
    exa
    nixfmt # *.nix files are used to pull in project deps, so we always need this
    ripgrep
    screen
  ];

  home.file = {
    ".profile".source = "${dotroot}/dotfiles/env";
    ".ssh/tlater.pub".source = "${dotroot}/keys/tlater.pub";
  };

  xdg.configFile."screen/config".source = "${dotroot}/dotfiles/screenrc";

  programs = {
    git = {
      enable = true;
      userName = "Tristan Daniël Maat";
      userEmail = "tm@tlater.net";
      signing.key = "0x49670FD774E43268";
      ignores = [ ".envrc" ".direnv/" ];
      extraConfig = {
        branch.autoSetupRebase = "always";
        checkout.defaultRemote = "origin";

        pull.rebase = true;
        pull.ff = "only";
        push.default = "current";

        # Magit-forge configuration
        github.user = "tlater";
        gitlab.user = "tlater";
        # gitlab.gitlab is intentional; tells magit-forge to use the
        # gitlab API and *then* specifies the domain
        "gitlab.gitlab.codethink.co.uk/api/v4".user = "tristanmaat";
        url."ssh://git@".pushInsteadOf = "https://";
      };
    };

    gpg = {
      enable = true;
      settings = {
        fixed-list-mode = true;
        keyid-format = "0xlong";
        personal-digest-preferences =
          builtins.concatStringsSep " " [ "SHA512" "SHA384" "SHA256" ];
        personal-cipher-preferences =
          builtins.concatStringsSep " " [ "AES256" "AES192" "AES" ];
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
        "tlater.net" = lib.hm.dag.entryAfter [ "*" ] {
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
