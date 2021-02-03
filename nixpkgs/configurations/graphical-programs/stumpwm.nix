{ pkgs, dotroot, ... }:

{
  home.packages = with pkgs; [
    local.stumpwm
    local.stumpwm-contrib

    # Allow us to change the running instance from emacs
    stumpish

    # Manages autostarts
    dex

    # Fonts
    hack-font
    noto-fonts
    noto-fonts-cjk
    noto-fonts-emoji
  ];

  home.file = { ".Xresources".source = "${dotroot}/dotfiles/Xresources"; };

  xdg.configFile = {
    "autostart/background.desktop".text = ''
      [Desktop Entry]
      Version=1.1
      Type=Application
      Name=Background
      GenericName=Background setter
      NoDisplay=true
      Comment=Set a desktop background; necessary because stumpwm overrides xprofile-set backgrounds
      Exec=${pkgs.local.background}/bin/background
    '';
    "fontconfig/fonts.conf".source = "${dotroot}/dotfiles/fonts.conf";
    "stumpwm/config" = {
      source = "${dotroot}/dotfiles/stumpwm/config";
      onChange =
        "${pkgs.local.stumpwm-contrib}/share/stumpwm/modules/util/stumpish/stumpish loadrc";
    };
  };

  fonts.fontconfig.enable = true;

  xsession = {
    enable = true;
    initExtra = ''
      export STUMPWM_CONTRIB_DIR=${pkgs.local.stumpwm-contrib}/share/stumpwm/modules
      export WM=stumpwm
      xrdb -merge ~/.Xresources
    '';
    windowManager.command = ''
      ${pkgs.local.stumpwm}/bin/stumpwm-lisp-launcher.sh \
        --eval '(require :asdf)' \
        --eval '(asdf:load-system :stumpwm)' \
        --eval '(stumpwm:stumpwm)'
    '';
  };
}
