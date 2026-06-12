/**
  Configuration related to desktop use.
*/
{ lib, pkgs, ... }: {
  imports = [
    ./desktop/flatpaks.nix
    ./desktop/greeter
    ./desktop/sway.nix
  ];

  i18n = {
    supportedLocales = [
      "en_US.UTF-8/UTF-8"
      "en_DK.UTF-8/UTF-8"
    ];

    extraLocaleSettings = {
      LC_MESSAGES = "en_US.UTF-8";
      LC_TIME = "en_DK.UTF-8";
    };
  };

  fonts = {
    enableDefaultPackages = true;

    packages = lib.attrValues {
      inherit (pkgs)
        hack-font
        noto-fonts
        noto-fonts-cjk-sans
        noto-fonts-color-emoji
        ;
    };

    fontconfig = {
      defaultFonts = {
        serif = [ "NotoSerif" ];
        sansSerif = [ "NotoSans" ];
        monospace = [ "Hack" ];
      };
    };
  };

  services = {
    # Required for gnome3 pinentry to work
    dbus.packages = [ pkgs.gcr ];

    libinput = {
      enable = true;
      mouse.middleEmulation = false;
    };

    # TODO: Is this still required?
    xserver = {
      enable = true;
      xkb.layout = "us";
    };
  };

  programs = {
    dconf.enable = true;

    uwsm = {
      enable = true;
      waylandCompositors.dummy = {
        prettyName = "dummy";
        comment = "Dummy service since the uwsm module won't work unless this is defined";
        binPath = "/";
      };
    };
  };
}
