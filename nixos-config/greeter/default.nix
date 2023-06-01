{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}: let
  hyprland-gtkgreet = pkgs.writeText "hyprland-gtkgreet" ''
    env = XDG_CACHE_HOME,/tmp

    exec-once = ${config.theming._hyprland-theme-init}/bin/hyprland-theme-init
    exec-once = eww -c ${./eww-config} open powermenu
    exec-once = gtkgreet -l; hyprctl dispatch exit

    decoration {
      blur = false
      drop_shadow = false
    }

    animations {
      enabled = false
    }

    misc {
      disable_hyprland_logo = yes
      disable_splash_rendering = yes
    }
  '';

  launch-gtkgreet = pkgs.writeShellApplication {
    name = "launch-gtkgreet";
    runtimeInputs = [
      flake-inputs.hyprland.packages.${pkgs.system}.hyprland-nvidia
      pkgs.hypr
      pkgs.greetd.gtkgreet
      pkgs.eww-wayland
    ];
    text = ''
      export XDG_SESSION_TYPE=wayland
      Hyprland -c ${hyprland-gtkgreet}
    '';
  };

  hyprland = pkgs.writeShellScriptBin "hyprland-run" ''
    export XDG_SESSION_TYPE=wayland
    systemd-cat -t xsession Hyprland
  '';

  hypr = pkgs.writeShellScriptBin "hypr-run" ''
    export XDG_SESSION_TYPE=x11
    ${config.services.xserver.displayManager.sessionData.wrapper} ${pkgs.hypr}/bin/Hypr
  '';
in {
  services.xserver.displayManager.lightdm.enable = false;

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = lib.getExe launch-gtkgreet;
    };
  };

  environment.systemPackages = with pkgs; [
    eww-wayland
    hypr
    hyprland
    pciutils
  ];

  fonts.fonts = [
    flake-inputs.self.packages.${pkgs.system}.phosphor-icons
  ];

  environment.etc."greetd/environments".text = ''
    hyprland-run
    hypr-run
  '';

  systemd.tmpfiles.rules = let
    user = config.services.greetd.settings.default_session.user;
  in [
    "d /var/log/gtkgreet 0755 greeter ${user} - -"
    "d /var/cache/gtkgreet 0755 greeter ${user} - -"
  ];
}
