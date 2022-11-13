{
  flake-inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf config.custom.is-nixos {
    nix = {
      package = pkgs.nixFlakes;
      extraOptions = ''
        experimental-features = nix-command flakes
      '';

      registry.nixpkgs = {
        from = {
          id = "nixpkgs";
          type = "indirect";
        };
        flake = flake-inputs.nixpkgs;
      };
    };

    nixpkgs.overlays = [
      flake-inputs.nurpkgs.overlay
    ];

    home.sessionVariables = {
      NIX_PATH = "nixpkgs=${flake-inputs.nixpkgs}";
    };
  };
}
