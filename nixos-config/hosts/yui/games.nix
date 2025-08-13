{ flake-inputs, pkgs, ... }:
let
  inherit (flake-inputs) nix-gaming nixpkgs-unstable;
  pkgsUnstable = nixpkgs-unstable.legacyPackages.${pkgs.system};
in
{
  imports = [
    nix-gaming.nixosModules.pipewireLowLatency
    nix-gaming.nixosModules.platformOptimizations
    nix-gaming.nixosModules.wine
  ];

  environment.systemPackages = [ pkgs.mangohud ];

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
      gamescopeSession.enable = true;
      # This sets some sensible game performance settings, along with
      # some required for Star Citizen
      platformOptimizations.enable = true;
    };

    gamescope = {
      enable = true;
      capSysNice = true;
      args = [
        "--steam"
        "--expose-wayland"
        "--rt"
        "-W 1920"
        "-H 1080"
        "--force-grab-cursor"
        "--grab"
        "--fullscreen"
      ];
    };

    wine = {
      enable = true;
      package = pkgsUnstable.wine-staging;
      ntsync = true;
    };
  };
}
