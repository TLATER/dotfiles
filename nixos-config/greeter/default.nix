{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}: let
  hyprland-gtkgreet = pkgs.writeText "hyprland-gtkgreet" ''
    env = XDG_CACHE_HOME,/tmp

    exec-once = eww -c ${./eww-config} open powermenu
    exec-once = gtkgreet -l; hyprctl dispatch exit
  '';

  launch-gtkgreet = pkgs.writeShellApplication {
    name = "launch-gtkgreet";
    runtimeInputs = [
      flake-inputs.hyprland.packages.${pkgs.system}.hyprland-nvidia
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
    hyprland
    pciutils
  ];

  fonts.fonts = [
    flake-inputs.self.packages.${pkgs.system}.phosphor-icons
  ];

  environment.etc."greetd/environments".text = ''
    hyprland-run
  '';

  systemd.tmpfiles.rules = let
    user = config.services.greetd.settings.default_session.user;
  in [
    "d /var/log/gtkgreet 0755 greeter ${user} - -"
    "d /var/cache/gtkgreet 0755 greeter ${user} - -"
  ];

  services.xserver.displayManager.session = [
    {
      manage = "desktop";
      name = "user-defined";
      start = config.services.xserver.displayManager.sessionData.wrapper;
    }
  ];
}
