{
  config,
  nixos-config ? { },
  pkgs,
  lib,
  ...
}:
let
  loginctl = "${pkgs.systemd}/bin/loginctl";
  swaymsg = "${nixos-config.programs.sway.package or pkgs.sway}/bin/swaymsg";
  systemctl = "${pkgs.systemd}/bin/systemctl";

  wpaperd-config = {
    default = {
      path = "~/Documents/Pictures/Backgrounds";
    };
  };

  wpaperd-config-dir = pkgs.runCommand "wpaperd-config" { } ''
    mkdir -p $out/wpaperd
    cp ${(pkgs.formats.toml { }).generate "wallpaper.toml" wpaperd-config} $out/wpaperd/wallpaper.toml
  '';

  keepassxc-copy = pkgs.writeShellApplication {
    name = "keepassxc-copy";
    runtimeInputs = with pkgs; [
      jq
      libsecret
      wl-clipboard
    ];
    text = ''
      WINDOW_TITLE="$(${swaymsg} --type get_tree | jq -r 'recurse(.nodes[]) | select(.focused)  | .app_id // .window_properties.title')"
      secret-tool lookup KP2A_URL "title://$WINDOW_TITLE" | wl-copy
      # Wait 45 seconds before clearing the clipboard
      sleep 45
      wl-copy -c
    '';
  };
in
{
  home.packages = [
    keepassxc-copy
    pkgs.glib
    pkgs.sway-contrib.grimshot
  ];

  wayland.windowManager.sway = {
    enable = true;
    package = null;
    config = null;
    systemd.xdgAutostart = true;
    extraConfigEarly = ''
      seat seat0 xcursor_theme Bibata-Original-Ice 24
    '';
    extraConfig = lib.fileContents ../../dotfiles/sway.conf;
  };

  services.swayidle = {
    enable = true;
    systemdTarget = "graphical-session.target";

    events = [
      {
        event = "lock";
        command = "${systemctl} --user start swaylock";
      }
    ];

    timeouts = [
      {
        timeout = 5 * 60;
        command = "${swaymsg} 'output * power off'";
        resumeCommand = "${swaymsg} 'output * power on'";
      }
      {
        timeout = 6 * 60;
        command = "${loginctl} lock-session";
      }
    ];
  };

  programs.swaylock = {
    enable = true;
    package = pkgs.swaylock-effects;
    settings = {
      screenshots = true;
      clock = true;
      indicator = true;
      indicator-radius = 100;
      indicator-thickness = 7;
      effect-blur = "7x5";
    };
  };

  programs.eww = {
    enable = true;
    configDir = ../../dotfiles/eww;
  };

  systemd.user.services.eww = {
    Unit = {
      Description = "System tray";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
      ExecStartPost = "${config.programs.eww.package}/bin/eww open tray";
    };
    Install.WantedBy = [ "graphical-session.target" ];
  };

  systemd.user.services.swaylock = {
    Unit.Description = "Lock screen";
    Service.ExecStart = "${config.programs.swaylock.package}/bin/swaylock";
  };

  systemd.user.services.wpaperd = {
    Unit = {
      Description = "Wallpaper daemon";
      After = [ "graphical-session-pre.target" ];
      PartOf = [ "graphical-session.target" ];
    };

    Service = {
      ExecStart = "${pkgs.wpaperd}/bin/wpaperd";
      Environment = "XDG_CONFIG_HOME=${wpaperd-config-dir}";
    };

    Install.WantedBy = [ "graphical-session.target" ];
  };
}
