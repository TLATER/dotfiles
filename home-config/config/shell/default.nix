{ config, pkgs, ... }:
{
  imports = [ ./zsh-config.nix ];

  xdg.configFile."screen/config".source = "${config._dotfiles}/screenrc";

  home.packages = with pkgs; [
    bat
    eza
    fd
    ouch
    ripgrep
    screen
  ];

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    maxCacheTtl = 2592000;
    pinentryPackage = pkgs.pinentry-gnome3;
  };

  programs = {
    ssh.enable = true;

    git = {
      enable = true;
      lfs.enable = true;
      userName = "Tristan Daniël Maat";

      ignores = [
        ".envrc"
        ".direnv/"
      ];
      extraConfig = {
        branch.autoSetupRebase = "always";
        checkout.defaultRemote = "origin";

        pull.rebase = true;
        pull.ff = "only";
        push.default = "current";

        init.defaultBranch = "main";
        submodule.recurse = "true";

        url."ssh://git@".pushInsteadOf = "https://";
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

      scdaemonSettings.disable-ccid = true;
    };
  };
}
