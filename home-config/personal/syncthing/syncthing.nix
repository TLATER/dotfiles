{
  config,
  inputs,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.services.syncthing;
  settingsFormat = pkgs.formats.json { };
in
{
  disabledModules = [ "services/syncthing.nix" ];

  options.services.syncthing = {
    settings = lib.mkOption {
      type = lib.types.submodule { freeformType = settingsFormat.type; };
      default = { };
    };
  };

  config = {
    services.syncthing.settings.options = {
      # Disable auto-ugprades; may not be necessary, but better safe
      # than sorry.
      autoUpgradeIntervalH = lib.mkDefault 0;

      # We do this via the systemd service (background.slice)
      setLowPriority = lib.mkDefault false;
    };

    xdg.configFile."systemd/user/syncthing.service".source =
      "${pkgs.syncthing}/share/systemd/user/syncthing.service";

    xdg.configFile."systemd/user/default.target.wants/syncthing.service" = {
      inherit (config.xdg.configFile."systemd/user/syncthing.service") source;
    };

    xdg.configFile."systemd/user/syncthing.service.d/override.conf".text = ''
      [Unit]
      Wants=syncthing-init.service

      [Service]
      Slice=background.slice
    '';

    systemd.user.services.syncthing-init = {
      Unit = {
        Description = "Syncthing configuration updater";
        Requisite = [ "syncthing.service" ];
        After = [ "syncthing.service" ];
      };

      Service = {
        Slice = "background.slice";
        Type = "oneshot";

        Environment = [ "NU_LOG_LEVEL=INFO" ];

        ExecStart =
          let
            configFile = settingsFormat.generate "config.json" cfg.settings.options;
          in
          inputs.self.builders.${pkgs.stdenv.hostPlatform.system}.writeNuWith {
            plugins = [ pkgs.nushellPlugins.query ];
            makeWrapperArgs = [
              "--add-flag"
              configFile
            ];
          } "syncthing-apply-config" ./syncthing-apply-config.nu;
      };
    };
  };
}
