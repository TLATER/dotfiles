{ pkgs, ... }:


let
  local-pkgs = import ../local-pkgs { inherit pkgs; };

in
{
  home.packages = with pkgs; [
    local-pkgs.stumpwm
    local-pkgs.stumpwm-contrib

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

  home.file = {
    ".Xresources".source = ../../dotfiles/Xresources;
  };

  xdg.configFile = {
    "autostart/background.desktop".text = ''
      [Desktop Entry]
      Version=1.1
      Type=Application
      Name=Background
      GenericName=Background setter
      NoDisplay=true
      Comment=Set a desktop background; necessary because stumpwm overrides xprofile-set backgrounds
      Exec=${local-pkgs.background}/bin/background
    '';
    "fontconfig/fonts.conf".source = ../../dotfiles/fonts.conf;
    "stumpwm/config" = {
      source = ../../dotfiles/stumpwm/config;
      onChange = "${local-pkgs.stumpwm-contrib}/share/stumpwm/modules/util/stumpish/stumpish loadrc";
    };
  };

  fonts.fontconfig.enable = true;

  xsession = {
    enable = true;
    initExtra = ''
      export STUMPWM_CONTRIB_DIR=${local-pkgs.stumpwm-contrib}/share/stumpwm/modules
      export WM=stumpwm
      xrdb -merge ~/.Xresources
    '';
    windowManager.command = ''
      ${local-pkgs.stumpwm}/bin/stumpwm-lisp-launcher.sh \
        --eval '(require :asdf)' \
        --eval '(asdf:load-system :stumpwm)' \
        --eval '(stumpwm:stumpwm)'
    '';
  };
}
