{
  config,
  flake-inputs,
  nixos-config ? { },
  pkgs,
  lib,
  ...
}:
let
  loginctl = "${pkgs.systemd}/bin/loginctl";
  swaymsg = "${nixos-config.programs.sway.package or pkgs.sway}/bin/swaymsg";
  systemctl = "${pkgs.systemd}/bin/systemctl";

  wpaperd-config = (pkgs.formats.toml { }).generate "wallpaper.toml" {
    default.path = "~/Documents/Pictures/Backgrounds";
  };

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
    systemd.enable = false;
    systemd.xdgAutostart = false;
    extraConfigEarly =
      let
        theme = "${
          flake-inputs.self.packages.${pkgs.system}.catppuccin-themes
        }/share/i3/themes/catppuccin-macchiato";
      in
      ''
        include ${theme}
        seat seat0 xcursor_theme Bibata-Original-Ice 24
      '';
    extraConfig = lib.fileContents ../dotfiles/sway.conf;
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

  programs = {
    fuzzel = {
      enable = true;
      settings = {
        main =
          let
            inherit (flake-inputs.self.packages.${pkgs.system}) catppuccin-themes;
          in
          {
            # The launch prefix *is* set correctly for terminals
            terminal = "${lib.getExe pkgs.alacritty} -e";
            width = "100";
            include = "${catppuccin-themes}/share/fuzzel/themes/catppuccin-macchiato/lavender.ini";
            icon-theme = config.gtk.iconTheme.name;
            font = "monospace:size=15";
          };
        border.radius = 0;
      };
    };

    swaylock = {
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

    eww = {
      enable = true;
      configDir = ../dotfiles/eww;
    };
  };

  systemd.user.services = {
    eww = {
      Unit = {
        Description = "System tray";
        After = [ "graphical-session.target" ];
        Before = [ "way-displays.service" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        ExecStart = "${config.programs.eww.package}/bin/eww daemon --no-daemonize";
        ExecStartPost = "${config.programs.eww.package}/bin/eww open tray";
        Environment = "PATH=${config.programs.eww.package}/bin:${pkgs.systemd}/bin:${pkgs.coreutils}/bin";
        Restart = "always";
        Type = "exec";
      };
      Install.WantedBy = [ "graphical-session.target" ];
    };

    swaylock = {
      Unit.Description = "Lock screen";
      Service.ExecStart = "${config.programs.swaylock.package}/bin/swaylock";
    };

    wpaperd = {
      Unit = {
        Description = "Wallpaper daemon";
        After = [ "graphical-session.target" ];
        Before = [ "way-displays.service" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
        X-Restart-Triggers = [ wpaperd-config ];
      };

      Service = {
        ExecStart = "${pkgs.wpaperd}/bin/wpaperd -c ${wpaperd-config}";
        Type = "exec";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
