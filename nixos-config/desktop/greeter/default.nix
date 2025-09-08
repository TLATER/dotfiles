{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}:
let
  sway = config.programs.sway.package;
  unsupported-gpu = lib.elem "nvidia" config.services.xserver.videoDrivers;

  sway-gtkgreet = pkgs.writeText "sway-gtkgreet" ''
    output '*' background #fafafa solid_color
    seat seat0 xcursor_theme Bibata-Original-Ice 24

    exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
    exec eww -c ${./eww-config} open powermenu
    exec "gtkgreet -l; swaymsg exit"
  '';

  launch-gtkgreet = pkgs.writeShellApplication {
    name = "launch-gtkgreet";
    runtimeInputs = [
      pkgs.eww
      pkgs.greetd.gtkgreet
      sway
    ];
    text = ''
      export XDG_SESSION_TYPE=wayland
      export HOME=/var/run/gtkgreet
      mkdir -p "$HOME/.cache"
      sway ${lib.optionalString unsupported-gpu "--unsupported-gpu"} -c ${sway-gtkgreet}
    '';
  };

  sway-run = pkgs.writeShellScriptBin "sway-run" ''
    uwsm start -S -F -- ${lib.getExe config.programs.sway.package} ${lib.optionalString unsupported-gpu "--unsupported-gpu"}
  '';
in
{
  services.xserver.displayManager.lightdm.enable = false;

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = lib.getExe launch-gtkgreet;
    };
  };

  environment.etc."greetd/environments".text = ''
    sway-run
  '';

  environment.systemPackages = with pkgs; [
    sway-run
    pciutils
  ];

  fonts.packages = [ flake-inputs.self.packages.${pkgs.system}.phosphor-icons ];

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
