{ config, lib, ... }:
let
  inherit (lib) types;
in
{
  options.unfree = {
    allowUnfreePackages = lib.mkOption {
      type = types.listOf types.str;
      default = [ ];
    };

    extraPredicates = lib.mkOption {
      type = types.listOf types.raw;
      default = [ ];
    };
  };

  config.nixpkgs.config.allowUnfreePredicate =
    pkg:
    lib.foldr (x: y: x || y) false (
      [ (lib.elem (lib.getName pkg) config.unfree.allowUnfreePackages) ]
      ++ (map (p: p pkg) config.unfree.extraPredicates)
    );
}
