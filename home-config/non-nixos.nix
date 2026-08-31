{ inputs, pkgs, ... }: {
  targets.genericLinux.enable = true;

  nix = {
    package = pkgs.lix;
    settings.experimental-features = [
      "nix-command"
      "flakes"
    ];

    registry.nixpkgs = {
      from = {
        id = "nixpkgs";
        type = "indirect";
      };
      flake = inputs.nixpkgs;
    };
  };

  home.sessionVariables = {
    NIX_PATH = "nixpkgs=${inputs.nixpkgs}";
  };
}
