{
  pkgs,
  config,
  lib,
  flake-inputs,
  ...
}:
let
  ewwBin = lib.getExe config.programs.eww.package;
  notificationsBin =
    lib.getExe' flake-inputs.self.packages.${pkgs.system}.desktop-logic
      "notifications";
in
{
  programs.eww = {
    enable = true;
    configDir = ../dotfiles/eww;
  };

  systemd.user.services.eww = {
    Unit = {
      Description = "System tray";
      After = [ "graphical-session.target" ];
      Before = [ "way-displays.service" ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${ewwBin} daemon --no-daemonize";
      ExecStartPost = "${ewwBin} open tray";
      Restart = "always";
      Type = "exec";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services.desktop-logic = {
    Unit = {
      Description = "Desktop logic daemon";
      After = [ "graphical-session.target" ];
      ConditionEnvironment = "WAYLAND_DISPLAY";
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = notificationsBin;
      Restart = "on-error";
      Type = "exec";
    };

    Install.WantedBy = [ "graphical-session.target" ];
  };
}
