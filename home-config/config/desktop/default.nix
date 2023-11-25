{
  config,
  lib,
  ...
}: {
  imports = [
    ./barrier.nix
    ./dunst.nix
    ./gtk.nix
    ./mime.nix
    ./ncmpcpp.nix
    ./pipewire.nix
    ./rofi.nix
    ./stumpwm.nix
    ./sway.nix
  ];

  config = lib.mkIf config.custom.desktop-environment {
    services.caffeine.enable = true;
    xsession.importedVariables = ["PATH"];

    assertions = [
      {
        assertion = config.custom.graphical-applications;
        message = "the desktop environment expects graphical applications to be set up as well";
      }
    ];
  };
}
