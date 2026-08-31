/**
  Configures a gtkgreet-based greetd greeter.
*/
{
  config,
  inputs,
  lib,
  pkgs,
  ...
}:
let
  /**
    A binary that launches gtkgreet inside sway.
  */
  sway-gtkgreet =
    (inputs.wrappers.lib.evalModule {
      imports = [ inputs.wrappers.lib.modules.default ];

      inherit pkgs;

      package = pkgs.sway;

      runtimePkgs = [
        pkgs.eww
        pkgs.gtkgreet
        config.programs.sway.package
      ];

      flags = {
        "--unsupported-gpu" = config.hardware.nvidia.enabled;
        "--config" = pkgs.writeText "sway-gtkgreet.conf" ''
          output '*' background #fafafa solid_color
          output 'GSS edid.build 0x00000001' disable
          seat seat0 xcursor_theme Bibata-Original-Ice 24

          exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
          exec eww -c ${./eww-config} open powermenu
          exec "gtkgreet -l; swaymsg exit"
        '';
      };

      env = {
        HOME = "/var/run/gtkgreet";
        XDG_SESSION_TYPE = "wayland";
      };

      runShell = [ "mkdir -p $HOME/.cache" ];
    }).config.wrapper;

  /**
    A binary that starts sway inside the uwsm session.
  */
  sway-run = pkgs.writeShellScriptBin "sway-run" ''
    uwsm start -F -- ${lib.getExe config.programs.sway.package} ${lib.optionalString config.hardware.nvidia.enabled "--unsupported-gpu"}
  '';
in
{
  # TODO: Is this really necessary?
  services.xserver.displayManager.lightdm.enable = true;

  services.greetd = {
    enable = true;
    settings.default_session.command = lib.getExe sway-gtkgreet;
  };

  environment = {
    etc."greetd/environments".text = ''
      sway-run
    '';

    systemPackages = [
      sway-run
      pkgs.pciutils
    ];
  };

  fonts.packages = [ inputs.self.packages.${pkgs.stdenv.hostPlatform.system}.phosphor-icons ];

  systemd.tmpfiles.rules =
    let
      inherit (config.services.greetd.settings.default_session) user;
    in
    [
      "d /run/gtkgreet 0755 greeter ${user} - -"
      "d /var/log/gtkgreet 0755 greeter ${user} - -"
      "d /var/cache/gtkgreet 0755 greeter ${user} - -"
    ];
}
