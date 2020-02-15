{ config, lib, pkgs, ... }:

let
  helpers = import ../helpers { inherit lib; };

in
{
  home.packages = with pkgs; [
    elinks
    neomutt
  ];

  home.file = {
    ".mailcap".source = ../../dotfiles/mailcap;
  };

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    neomutt = {
      enable = true;
      extraConfig = builtins.readFile ../../dotfiles/neomutt/neomuttrc;
    };
  };

  services = {
    mbsync.enable = true;
  };

  accounts = {
    email = {
      accounts = {
        "codethink.co.uk" = {
          address = "tristan.maat@codethink.co.uk";
          primary = config.isWorkProfile;
          realName = "Tristan Daniël Maat";

          userName = "tristanmaat";
          passwordCommand = "PASSWORD_STORE_DIR=${config.xdg.dataHome}/password-store ${pkgs.pass}/bin/pass codethink.co.uk | ${pkgs.coreutils}/bin/tr -d '\\n'";
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
            enable = config.isWorkProfile;
          };
          msmtp = {
            enable = config.isWorkProfile;
            extraConfig = {
              from = "tristan.maat@codethink.co.uk";
            };
          };
          neomutt = {
            enable = config.isWorkProfile;
            sendMailCommand = "msmtp --read-recipients";
            extraConfig = ''
              mailboxes `find ${config.accounts.email.maildirBasePath}/codethink.co.uk/* -type d ! \( -name new -or -name cur -or -name tmp \) -printf '"%p" '`
            '';
          };
        };
      };
      maildirBasePath = "${config.xdg.dataHome}/mail";
    };
  };
}
