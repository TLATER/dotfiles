{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  hyprctl = "${flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.hyprland}/bin/hyprctl";
  loginctl = "${pkgs.systemd}/bin/loginctl";

  wpaperd-config = {
    default = {
      path = "~/Documents/Pictures/Backgrounds";
    };
  };

  wpaperd-config-dir = pkgs.runCommand "wpaperd-config" {} ''
    mkdir -p $out/wpaperd
    cp ${(pkgs.formats.toml {}).generate "wallpaper.toml" wpaperd-config} $out/wpaperd/wallpaper.toml
  '';

  keepassxc-copy = pkgs.writeShellApplication {
    name = "keepassxc-copy";
    runtimeInputs = with pkgs; [
      jq
      libsecret
      wl-clipboard
    ];

    text = ''
      WINDOW_TITLE="$('${hyprctl}' -j activewindow | jq -r '.title')"
      secret-tool lookup KP2A_URL "title://$WINDOW_TITLE" | wl-copy
      # Wait 45 seconds before clearing the clipboard
      sleep 45
      wl-copy -c
    '';
  };
in {
  config = lib.mkIf config.custom.desktop-environment {
    home.packages = with pkgs; [
      slurp
      grim
      keepassxc-copy
      flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.grimblast
    ];

    systemd.user.targets.hyprland-session = {
      Unit = {
        Description = "Hyprland compositor session";
        Documentation = ["man:systemd.special(7)"];
        BindsTo = ["graphical-session.target"];
        Wants = ["graphical-session-pre.target"];
        After = ["graphical-session-pre.target"];
      };
    };

    xdg.configFile."hypr/hyprland.conf" = {
      source = ../../dotfiles/hyprland.conf;
      onChange = let
        inherit (flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}) hyprland;
      in ''
        (  # execute in subshell so that `shopt` won't affect other scripts
          shopt -s nullglob  # so that nothing is done if /tmp/hypr/ does not exist or is empty
          for instance in /tmp/hypr/*; do
            HYPRLAND_INSTANCE_SIGNATURE=''${instance##*/} ${hyprland}/bin/hyprctl reload config-only \
              || true  # ignore dead instance(s)
          done
        )
      '';
    };

    services.swayidle = {
      enable = true;
      systemdTarget = "graphical-session.target";

      events = [
        {
          event = "lock";
          command = "${config.programs.swaylock.package}/bin/swaylock";
        }
      ];

      timeouts = [
        {
          timeout = 5 * 60;
          command = "${hyprctl} dispatch dpms off";
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

    systemd.user.services.wpaperd = {
      Unit = {
        Description = "Wallpaper daemon";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };

      Service = {
        ExecStart = "${pkgs.wpaperd}/bin/wpaperd --no-daemon";
        Environment = "XDG_CONFIG_HOME=${wpaperd-config-dir}";
      };

      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
