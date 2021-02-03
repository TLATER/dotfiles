{ config, pkgs, ... }:

{
  accounts.email.accounts = {
    "tlater.net" = {
      address = "tm@tlater.net";
      primary =
        !(builtins.hasAttr "codethink.co.uk" config.accounts.email.accounts);
      realName = "Tristan Daniël Maat";

      userName = "tlater";
      passwordCommand =
        "PASSWORD_STORE_DIR=${config.xdg.dataHome}/password-store ${pkgs.pass}/bin/pass protonmail/local | ${pkgs.coreutils}/bin/tr -d '\\n'";

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
        extraConfig.account.AuthMechs = "LOGIN";
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
