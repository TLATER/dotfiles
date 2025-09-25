{
  pkgs,
  config,
  lib,
  ...
}:
let
  ewwBin = lib.getExe config.programs.eww.package;
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
      Environment = "PATH=${
        lib.makeBinPath [
          config.programs.eww.package
          pkgs.systemd
          pkgs.coreutils
        ]
      }";
      Restart = "always";
      Type = "exec";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };
}
