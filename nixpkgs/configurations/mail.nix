{ config, lib, pkgs, ... }:

let
  helpers = import ../helpers { inherit lib; };

in
{
  home.packages = with pkgs; [
    neomutt
  ];

  home.file = {
    ".mailcap".source = ../../dotfiles/mailcap;
  };

  xdg.configFile = {
    "neomutt" = {
      recursive = true;
      source = ../../dotfiles/neomutt;
    };
  };

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
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
            enable = config.isWorkProfile;
          };
          msmtp = {
            enable = config.isWorkProfile;
            extraConfig = {
              from = "tristan.maat@codethink.co.uk";
            };
          };
        };
      };
      maildirBasePath = "${config.xdg.dataHome}/mail";
    };
  };
}
