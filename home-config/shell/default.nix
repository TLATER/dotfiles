{ config, pkgs, ... }:
{
  imports = [ ./nushell.nix ];

  xdg = {
    configFile."zellij".source = ../dotfiles/zellij;
    configFile."dashrc".source = ../dotfiles/dashrc;
  };

  home = {
    packages = with pkgs; [
      any-nix-shell
      bat
      eza
      fd
      ouch
      ripgrep
      zellij
    ];

    file.".profile".text = ''
      . "${config.home.profileDirectory}/etc/profile.d/hm-session-vars.sh"

      export BROWSER='librewolf'
      export VISUAL='emacsclient'
      export EDITOR='emacsclient'
      export ALTERNATE_EDITOR='emacs'
      export VTERM='alacritty'
      export ENV='${config.xdg.configHome}/dashrc'
    '';
  };

  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
    defaultCacheTtl = 86400;
    maxCacheTtl = 2592000;
    pinentry.package = pkgs.pinentry-gnome3;
  };

  programs = {
    ssh.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
    };

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
