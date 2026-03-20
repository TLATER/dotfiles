{
  config,
  pkgs,
  lib,
  ...
}:
{
  options.services.udev.rules = lib.mkOption {
    default = { };
    type = lib.types.attrsOf lib.types.lines;
  };

  config = lib.mkIf (config.services.udev.rules != { }) {
    services.udev.packages = lib.mapAttrsToList (
      name: text:
      pkgs.writeTextFile {
        inherit name;
        inherit text;
        destination = "/lib/udev/rules.d/${name}";

        checkPhase = ''
          ${lib.getExe' pkgs.systemd "udevadm"} verify $out/lib/udev/rules.d/${name}
        '';
      }
    ) config.services.udev.rules;
  };
}
