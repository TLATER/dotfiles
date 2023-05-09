{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
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
      flake-inputs.hyprland.packages.${pkgs.system}.hyprland-nvidia
    ];

    text = ''
      WINDOW_TITLE="$(hyprctl -j activewindow | jq -r '.title')"
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
      flake-inputs.hyprland-contrib.packages.${pkgs.system}.grimblast
    ];

    wayland.windowManager.hyprland = {
      enable = true;
      systemdIntegration = true;
      nvidiaPatches = true;

      extraConfig = builtins.readFile ../../dotfiles/hyprland.conf;
    };

    systemd.user.services.wpaperd = {
      Unit = {
        Description = "Wallpaper daemon";
        After = ["graphical-session-pre.target"];
        PartOf = ["graphical-session.target"];
      };

      Service = {
        ExecStart = "${flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.wpaperd}/bin/wpaperd --no-daemon";
        Environment = "XDG_CONFIG_HOME=${wpaperd-config-dir}";
      };

      Install.WantedBy = ["graphical-session.target"];
    };
  };
}
