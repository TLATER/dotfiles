{ config, pkgs, ... }:

{
  imports = [ ./. ];

  accounts.email.accounts = {
    "codethink.co.uk" = {
      address = "tristan.maat@codethink.co.uk";
      primary = true;
      realName = "Tristan Daniël Maat";

      userName = "tristanmaat";
      passwordCommand =
        "PASSWORD_STORE_DIR=${config.xdg.dataHome}/password-store ${pkgs.pass}/bin/pass work/codethink.co.uk | ${pkgs.coreutils}/bin/head -n 1 | ${pkgs.coreutils}/bin/tr -d '\\n'";
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
        enable = true;
      };
      msmtp = {
        enable = true;
        extraConfig.from = "tristan.maat@codethink.co.uk";
      };
      neomutt = {
        enable = true;
        sendMailCommand = "msmtp --read-recipients";
        extraConfig = ''
          mailboxes `find ${config.accounts.email.maildirBasePath}/codethink.co.uk/* -type d ! \( -name new -or -name cur -or -name tmp \) -printf '"%p" '`
        '';
      };
    };
  };

  services.mbsync.enable = true;
}
