{
  config,
  flake-inputs,
  nixos-config ? { },
  pkgs,
  lib,
  ...
}:
let
  inherit (flake-inputs.self.pkgs-lib.${pkgs.stdenv.hostPlatform.system}) writeNuBinWith;

  loginctl = "${pkgs.stdenv.hostPlatform.system}/bin/loginctl";
  swaypkg = nixos-config.programs.sway.package or pkgs.sway;
  systemctl = "${pkgs.stdenv.hostPlatform.system}/bin/systemctl";

  keepassxc-copy =
    writeNuBinWith
      {
        packages = [
          pkgs.jq
          pkgs.libsecret
          pkgs.wl-clipboard

          swaypkg
        ];
      }
      "keepassxc-copy"
      ''
        let current_window = swaymsg --type get_tree | jq 'recurse(.nodes[]) | select(.focused?)'
        let title = $current_window.app_id | default $current_window.window_properties.title

        secret-tool lookup KP2A_URL $'title://($title)' | wl-copy
        sleep 45sec
        wl-copy -c
      '';
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
          flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.catppuccin-themes
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
        command = "${lib.getExe swaypkg} 'output * power off'";
        resumeCommand = "${lib.getExe swaypkg} 'output * power on'";
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
            inherit (flake-inputs.self.packages.${pkgs.stdenv.hostPlatform.system}) catppuccin-themes;
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
  };

  systemd.user.services = {
    swaylock = {
      Unit.Description = "Lock screen";
      Service.ExecStart = "${config.programs.swaylock.package}/bin/swaylock";
    };

    swaybg = {
      Unit = {
        Description = "Wallpaper daemon";
        After = [ "graphical-session.target" ];
        Before = [ "way-displays.service" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
      };

      Service = {
        Slice = "background-graphical.slice";
        ExecStart =
          (flake-inputs.self.pkgs-lib.${pkgs.stdenv.hostPlatform.system}.writeNuWith
            { packages = [ pkgs.swaybg ]; }
            "swaybg"
            ''
              let background = ls ~/Documents/Pictures/Backgrounds | shuffle | first | $in.name
              exec swaybg --output=* --image $background
            ''
          ).outPath;
        Type = "exec";
      };

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
