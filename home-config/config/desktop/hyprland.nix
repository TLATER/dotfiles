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
in {
  config = lib.mkIf config.custom.desktop-environment {
    home.packages = with pkgs; [
      slurp
      grim
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
