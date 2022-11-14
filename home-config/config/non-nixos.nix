{
  flake-inputs,
  config,
  lib,
  pkgs,
  ...
}: {
  config = lib.mkIf (!config.custom.is-nixos) {
    targets.genericLinux.enable = true;

    # zsh doesn't always load ~/.profile on other distros
    programs.zsh.envExtra = ''
      source "$HOME/.profile"
    '';

    nix = {
      package = pkgs.nixFlakes;
      settings.experimental-features = ["nix-command" "flakes"];

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
