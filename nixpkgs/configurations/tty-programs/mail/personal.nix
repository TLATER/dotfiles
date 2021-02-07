{ config, pkgs, ... }:

{
  # Work around https://github.com/nix-community/home-manager/issues/249
  systemd.user.services.mbsync.Service.Environment =
    "PATH=${pkgs.sops}/bin:${pkgs.gnupg}/bin";

  accounts.email.accounts = {
    "tlater.net" = {
      address = "tm@tlater.net";
      primary =
        !(builtins.hasAttr "codethink.co.uk" config.accounts.email.accounts);
      realName = "Tristan Daniël Maat";

      userName = "tlater";
      passwordCommand = "${pkgs.local.read-sops}/bin/read-sops hydroxide";

      imap = {
        host = "127.0.0.1";
        port = 1143;
        tls.enable = false;
      };
      smtp = {
        host = "localhost";
        port = 1025;
        tls.enable = false;
      };
      gpg = {
        key = "0x35AED29F3800E029";
        signByDefault = true;
      };

      mbsync = {
        create = "maildir";
        expunge = "both";
        remove = "maildir";
        enable = true;
      };
      msmtp = {
        enable = true;
        extraConfig = {
          from = "tm@tlater.net";
          auth = "plain";
        };
      };
      neomutt = {
        enable = true;
        sendMailCommand = "msmtp --read-recipients";
        extraConfig = ''
          set pgp_default_key = 0x35AED29F3800E029
          mailboxes `find ${config.accounts.email.maildirBasePath}/tlater.net/* -type d ! \( -name new -or -name cur -or -name tmp \) -printf '"%p" '`
        '';
      };
    };
  };

  systemd.user.services = {
    hydroxide = {
      Unit.Description = "Protonmail service proxy";
      Service.ExecStart = "${pkgs.hydroxide}/bin/hydroxide serve";
      Install.WantedBy = [ "mbsync.service" ];
    };
  };

  services.mbsync.enable = true;
}
