{
  config,
  lib,
  pkgs,
  ...
}:
let
  cfg = config.services.wayDisplays;
  settingsFormat = pkgs.formats.yaml { };
in
{
  options.services.wayDisplays = {
    enable = lib.mkEnableOption "wayDisplays";

    settings = lib.mkOption {
      inherit (settingsFormat) type;
      default = { };
      description = ''
        The way-displays configuration. Refer to
        <https://github.com/alex-courtis/way-displays/wiki/Configuration> for
        details on supported values.
      '';
    };
  };

  config = lib.mkIf cfg.enable {
    # For the client binary
    home.packages = [ pkgs.way-displays ];

    systemd.user.services.way-displays = {
      Unit = {
        Description = "Display manager";
        After = [ "graphical-session.target" ];
        ConditionEnvironment = "WAYLAND_DISPLAY";
        PartOf = [ "graphical-session.target" ];
      };

      Service.ExecStart =
        let
          exe = lib.getExe pkgs.way-displays;
          settings = settingsFormat.generate "way-display-cfg.yaml" cfg.settings;
        in
        "${exe} -c ${settings}";

      Install.WantedBy = [ "graphical-session.target" ];
    };
  };
}
