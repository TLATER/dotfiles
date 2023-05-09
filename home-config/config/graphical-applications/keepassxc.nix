{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf config.custom.graphical-applications {
    home.packages = with pkgs; [
      keepassxc
    ];
  };
}
