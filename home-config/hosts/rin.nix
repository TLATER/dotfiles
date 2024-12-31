{
  imports = [
    ../config
    ../config/applications/graphical
    ../config/applications/tty
    ../config/desktop
    ../config/services
    ../config/shell
    ../config/xdg-settings.nix

    ../config/work/famedly.nix
  ];

  # Adaptive sync doesn't work on rin
  wayland.windowManager.sway.extraConfig = ''
    output "*" adaptive_sync off
  '';
}
