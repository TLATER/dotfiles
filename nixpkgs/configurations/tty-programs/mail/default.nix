{
  config,
  pkgs,
  dotroot,
  ...
}: let
  markdown-script = pkgs.writeScript "convert-mail.sh" ''
    #!${pkgs.bash}/bin/bash
    ${pkgs.pandoc}/bin/pandoc \
        --from markdown \
        --to html \
        -o /tmp/neomutt-alternative.html
  '';
in {
  home.packages = with pkgs; [
    lynx
    neomutt

    local.dump-ics
  ];

  xdg.configFile."mailcap".source = "${dotroot}/dotfiles/mailcap";

  programs = {
    mbsync.enable = true;
    msmtp.enable = true;
    neomutt = {
      enable = true;
      editor = "$EDITOR -c";
      extraConfig = ''
        source "${dotroot}/dotfiles/neomutt/neomuttrc"
        set new_mail_command = "${pkgs.libnotify}/bin/notify-send 'New mail in %f' '%n new messages, %u unread.'"
        macro compose K "| ${markdown-script}<Enter><attach-file>/tmp/neomutt-alternative.html<Enter>"
      '';
    };
  };

  accounts.email.maildirBasePath = "${config.xdg.dataHome}/mail";
}
