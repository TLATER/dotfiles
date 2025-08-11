{
  config,
  pkgs,
  lib,
  ...
}:
let
  cfg = config.home.cleanGenerations;
in
{
  options.home.cleanGenerations = {
    enable = lib.mkEnableOption "clean obsolete home-manager generations on switch";
  };

  config = lib.mkIf cfg.enable {
    home.activation.expireOldGenerations = lib.hm.dag.entryAfter [
      "writeBoundary"
    ] (pkgs.writers.writeNu "clean-generations.nu" (builtins.readFile ./clean-generations.nu)).outPath;
  };
}
