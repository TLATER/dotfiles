{
  config,
  lib,
  pkgs,
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
  };

  config = lib.mkIf (config.theming.cursor.theme != null) {
    programs.sway.extraSessionCommands = let
      cursorThemeInit = pkgs.writeShellApplication {
        name = "cursor-theme-init";
        runtimeInputs = with pkgs; [
          glib
          xorg.xsetroot
        ];
        text = ''
          xsetroot -cursor_name left_ptr
          export XDG_DATA_DIRS=${pkgs.gsettings-desktop-schemas}/share/gsettings-schemas/gsettings-desktop-schemas-44.0
          gsettings set org.gnome.desktop.interface cursor-theme '${config.theming.cursor.theme}'
          gsettings set org.gnome.desktop.interface cursor-size '${toString config.theming.cursor.size}'
        '';
      };
    in
      lib.concatStringsSep "\n" [
        ''
          export XCURSOR_THEME='${config.theming.cursor.theme}'
          export XCURSOR_SIZE=${toString (builtins.floor (config.theming.cursor.size * config.theming.cursor.x-scaling))}
        ''
        cursorThemeInit
      ];
  };
}
