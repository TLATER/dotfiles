{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  tlaterpkgs = flake-inputs.self.packages.${pkgs.system};
in {
  config = lib.mkIf config.custom.desktop-environment {
    home.packages = with pkgs; [
      tlaterpkgs.stumpwm
      tlaterpkgs.stumpwm-contrib

      # Allow us to change the running instance from emacs
      stumpish

      # Manages autostarts
      dex

      # Takes screenshots
      tlaterpkgs.cap

      # Fonts
      hack-font
      noto-fonts
      noto-fonts-cjk
      noto-fonts-emoji
    ];

    home.file.".Xresources".source = "${config._dotfiles}/Xresources";

    xdg.configFile = {
      "autostart/background.desktop".text = ''
        [Desktop Entry]
        Version=1.1
        Type=Application
        Name=Background
        GenericName=Background setter
        NoDisplay=true
        Comment=Set a desktop background; necessary because stumpwm overrides xprofile-set backgrounds
        Exec=${tlaterpkgs.background}/bin/background
      '';
      "fontconfig/fonts.conf".source = "${config._dotfiles}/fonts.conf";
      "stumpwm/config".source = "${config._dotfiles}/stumpwm/config";
    };

    fonts.fontconfig.enable = true;

    xsession = {
      enable = true;
      initExtra = ''
        export STUMPWM_CONTRIB_DIR=${tlaterpkgs.stumpwm-contrib}/share/stumpwm/modules
        xrdb -merge ~/.Xresources

        if [ "$XDG_CURRENT_DESKTOP" = "default" ]; then
            export XDG_CURRENT_DESKTOP=stumpwm
        fi
      '';
      windowManager.command = ''
        if [ "$XDG_CURRENT_DESKTOP" = "stumpwm" ]; then
            ${tlaterpkgs.stumpwm}/bin/stumpwm
        else
            eval "$@"
        fi
      '';
    };
  };
}
