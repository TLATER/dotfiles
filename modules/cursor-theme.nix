{
  config,
  lib,
  pkgs,
  flake-inputs,
  ...
}: let
  inherit (lib.types) string nullOr number;
in {
  options.theming = {
    cursor = {
      theme = lib.mkOption {
        default = null;
        type = nullOr string;
        description = "The default cursor theme to use. Package must be installed separately";
      };

      size = lib.mkOption {
        default = 24;
        type = number;
        description = "The size in pixels with which to render the cursor";
      };

      x-scaling = lib.mkOption {
        default = 1.0;
        type = number;
        description = "How much to scale the cursor size by on xwayland applications";
      };
    };

    _hyprland-theme-init = lib.mkOption {
      default = pkgs.writeShellApplication {
        name = "hyprland-theme-init";
        runtimeInputs = [
          pkgs.glib
          pkgs.xorg.xsetroot
          flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.hyprland
        ];
        text = ''
          xsetroot -cursor_name left_ptr
          hyprctl setcursor '${config.theming.cursor.theme}' ${toString config.theming.cursor.size}
          export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/gsettings-desktop-schemas-44.0
          gsettings set org.gnome.desktop.interface cursor-theme '${config.theming.cursor.theme}'
          gsettings set org.gnome.desktop.interface cursor-size '${toString config.theming.cursor.size}'
        '';
      };
    };
  };

  config = lib.mkIf (config.theming.cursor.theme != null) {
    environment.etc."hyprland-theme-init".source = "${config.theming._hyprland-theme-init}/bin/hyprland-theme-init";

    environment.extraInit = ''
      export XCURSOR_THEME='${config.theming.cursor.theme}'
      export XCURSOR_SIZE=${toString (builtins.floor (config.theming.cursor.size * config.theming.cursor.x-scaling))}
    '';
  };
}
