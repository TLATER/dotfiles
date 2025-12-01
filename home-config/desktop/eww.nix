{
  flake-inputs,
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

  systemd.user.services = {
    eww = {
      Unit = {
        Description = "System tray";
        After = [ "graphical-session.target" ];
        Before = [ "way-displays.service" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
        Requires = [ "dbus.socket" ];
      };

      Service = {
        ExecStart = "${ewwBin} daemon --no-daemonize";
        ExecStartPost = "${ewwBin} open tray";
        Environment = "PATH=${
          lib.makeBinPath [
            pkgs.bashInteractive # `eww` shells out using `sh`; `dash` does not currently contain an `sh` symlink
            pkgs.nushell
            pkgs.systemd
          ]
        }";
        Restart = "always";
        Type = "exec";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };

    desktop-logic = {
      Unit = {
        Description = "Desktop logic";
        After = [ "graphical-session.target" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = lib.getExe flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.desktop-logic;
        Restart = "on-failure";
        Type = "dbus";
        BusName = "net.tlater.DesktopLogic";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
