{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (flake-inputs.self.packages.${pkgs.system}) dump-ics;

  markdown-script = pkgs.writeScript "convert-mail.sh" ''
    #!${pkgs.bash}/bin/bash
    ${pkgs.pandoc}/bin/pandoc \
        --from markdown \
        --to html \
        -o /tmp/neomutt-alternative.html
  '';
in {
  config = lib.mkIf (config.custom.is-work && config.custom.desktop-environment) {
    home.packages = with pkgs; [
      lynx
      neomutt

      dump-ics
    ];

    xdg.configFile."mailcap".source = "${config._dotfiles}/dotfiles/mailcap";

    services.mbsync.enable = true;

    programs = {
      mbsync.enable = true;
      msmtp.enable = true;
      neomutt = {
        enable = true;
        editor = "$EDITOR -c";
        extraConfig = ''
          source "${config._dotfiles}/dotfiles/neomutt/neomuttrc"
          set new_mail_command = "${pkgs.libnotify}/bin/notify-send 'New mail in %f' '%n new messages, %u unread.'"
          macro compose K "| ${markdown-script}<Enter><attach-file>/tmp/neomutt-alternative.html<Enter>"
        '';
      };
    };

    accounts.email = {
      maildirBasePath = "${config.xdg.dataHome}/mail";

      accounts = {
        "codethink.co.uk" = {
          address = "tristan.maat@codethink.co.uk";
          primary = true;
          realName = "Tristan Daniël Maat";

          userName = "tristanmaat";
          passwordCommand = "PASSWORD_STORE_DIR=${config.xdg.dataHome}/password-store ${pkgs.pass}/bin/pass work/codethink.co.uk | ${pkgs.coreutils}/bin/head -n 1 | ${pkgs.coreutils}/bin/tr -d '\\n'";
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
            expunge = "both";
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
    };
  };
}
