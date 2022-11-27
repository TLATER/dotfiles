{
  pkgs,
  config,
  lib,
  ...
}: {
  config = lib.mkIf config.custom.desktop-environment {
    gtk = {
      enable = true;

      font = {
        package = pkgs.noto-fonts;
        name = "NotoSans";
      };

      theme = {
        package = pkgs.qogir-theme;
        name = "Qogir-Dark";
      };
    };
  };
}
