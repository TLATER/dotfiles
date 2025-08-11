{ pkgs, flake-inputs, ... }:
{
  home.shell.enableNushellIntegration = true;

  programs.nushell = {
    enable = true;
    configFile.source = ../dotfiles/nushell/config.nu;
  };

  programs.carapace = {
    enable = true;
    # Needed because of https://github.com/nix-community/home-manager/issues/7517
    package = flake-inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.carapace;
  };
}
