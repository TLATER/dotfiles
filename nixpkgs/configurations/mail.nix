{ config, lib, pkgs, ... }:

let
  helpers = import ../helpers { inherit lib; };
  local-pkgs = import ../local-pkgs { inherit pkgs; };
  # TODO: Think a bit about whether we can add a minimal bit of CSS
  # here
  markdown-script = pkgs.writeScript "convert-mail.sh" ''
    #!${pkgs.bash}/bin/bash
    ${pkgs.pandoc}/bin/pandoc \
        --from markdown \
        --to html \
        -o /tmp/neomutt-alternative.html
  '';

in
{
  home.packages = with pkgs; [
    elinks
    hydroxide
    neomutt

    local-pkgs.dump-ics
  ];

  home.file = {
    ".mailcap".source = ../../dotfiles/mailcap;
  };

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    neomutt = {
      enable = true;
      editor = "$EDITOR -c";
      extraConfig = builtins.readFile ../../dotfiles/neomutt/neomuttrc + ''
        macro compose K "| ${markdown-script}<Enter><attach-file>/tmp/neomutt-alternative.html<Enter>"
      '';
    };
  };

  services = {
    mbsync.enable = true;
  };

  systemd.user.services = {
    hydroxide = {
      Unit = {
        Description = "Protonmail service proxy";
      };

      Service = {
        ExecStart = "${pkgs.hydroxide}/bin/hydroxide serve";
      };

      Install = {
        WantedBy = [ "mbsync.service" ];
      };
    };
  };

  accounts = {
    email = {
      accounts = {
        "tlater.net" = {
          address = "tm@tlater.net";
          primary = !config.isWorkProfile;
          realName = "Tristan Daniël Maat";

          userName = "tlater";
          passwordCommand = "PASSWORD_STORE_DIR=${config.xdg.dataHome}/password-store ${pkgs.pass}/bin/pass protonmail/local | ${pkgs.coreutils}/bin/tr -d '\\n'";
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
