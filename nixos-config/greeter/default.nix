{
  config,
  pkgs,
  lib,
  flake-inputs,
  ...
}: let
  sway = config.programs.sway.package;
  unsupported-gpu = lib.elem "nvidia" config.services.xserver.videoDrivers;

  sway-gtkgreet = pkgs.writeText "sway-gtkgreet" ''
    output '*' background #fafafa solid_color

    exec dbus-update-activation-environment --systemd DISPLAY WAYLAND_DISPLAY SWAYSOCK
    exec ${pkgs.eww-wayland}/bin/eww -c ${./eww-config} open powermenu
    exec "${pkgs.greetd.gtkgreet}/bin/gtkgreet -l; ${sway}/bin/swaymsg exit"
  '';

  launch-gtkgreet = pkgs.writeShellApplication {
    name = "launch-gtkgreet";
    runtimeInputs = [
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
    export XDG_SESSION_TYPE=wayland
    systemd-cat -t xsession sway ${lib.optionalString unsupported-gpu "--unsupported-gpu"}
  '';
in {
  services.xserver.displayManager.lightdm.enable = false;

  services.greetd = {
    enable = true;
    settings.default_session = {
      command = lib.getExe launch-gtkgreet;
    };
  };

  environment.systemPackages = with pkgs; [
    eww-wayland
    sway-run
    pciutils
  ];

  fonts.fonts = [
    flake-inputs.self.packages.${pkgs.system}.phosphor-icons
  ];

  environment.etc."greetd/environments".text = ''
    sway-run
  '';

  systemd.tmpfiles.rules = let
    inherit (config.services.greetd.settings.default_session) user;
  in [
    "d /run/gtkgreet 0755 greeter ${user} - -"
    "d /var/log/gtkgreet 0755 greeter ${user} - -"
    "d /var/cache/gtkgreet 0755 greeter ${user} - -"
  ];
}
