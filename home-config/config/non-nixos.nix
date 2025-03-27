{ flake-inputs, pkgs, ... }:
{
  targets.genericLinux.enable = true;

  # zsh doesn't always load ~/.profile on other distros
  programs.zsh.envExtra = ''
    source "$HOME/.profile"
  '';

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
      flake = flake-inputs.nixpkgs;
    };
  };

  nixpkgs.overlays = [ flake-inputs.nurpkgs.overlays.default ];

  home.sessionVariables = {
    NIX_PATH = "nixpkgs=${flake-inputs.nixpkgs}";
  };
}
