{
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (!config.custom.is-work) {
    home.packages = with pkgs; [prismlauncher];
  };
}
