{ flake-inputs, pkgs, ... }:
let
  inherit (flake-inputs) nix-gaming nixpkgs-unstable;
  pkgsUnstable = nixpkgs-unstable.legacyPackages.${pkgs.system};
  pkgsGames = nix-gaming.packages.${pkgs.system};
in
{
  imports = [
    nix-gaming.nixosModules.pipewireLowLatency
    nix-gaming.nixosModules.platformOptimizations
    nix-gaming.nixosModules.wine
  ];

  environment.systemPackages =
    let
      osuLazer = pkgsGames.osu-lazer-bin.override { gmrun_enable = false; };
    in
    [ osuLazer ];

  # Make steam controller work
  hardware.steam-hardware.enable = true;

  services = {
    joycond.enable = true;
    pipewire.lowLatency.enable = true;
  };

  programs = {
    steam = {
      enable = true;
      extraCompatPackages = [ pkgsUnstable.proton-ge-bin ];
      # This sets some sensible game performance settings, along with
      # some required for Star Citizen
      platformOptimizations.enable = true;
    };

    wine = {
      enable = true;
      package = pkgsUnstable.wine-staging;
      ntsync = true;
    };
  };
}
