{ flake-inputs, pkgs, ... }:
let
  inherit (flake-inputs) nix-gaming nixpkgs-unstable self;
  pkgsUnstable = nixpkgs-unstable.legacyPackages.${pkgs.system};
  pkgsGames = nix-gaming.packages.${pkgs.system};
  pkgsSelf = self.packages.${pkgs.system};
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
    [
      osuLazer

      pkgs.prismlauncher

      pkgsSelf.edopro
      pkgsSelf.jazz-jackrabbit-2
    ];

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
      protontricks.enable = true;
    };

    wine = {
      enable = true;
      package = pkgsUnstable.wine-staging;
      ntsync = true;
    };

    gamescope = {
      enable = true;

      # Since the steam overlay currently borks gamescope, use in
      # steam's launch options:
      #
      # LD_PRELOAD= gamescope -- env LD_PRELOAD="$LD_PRELOAD" %command%

      args = [
        "--output-width 1920"
        "--output-height 1080"
        "--fullscreen"
        "--adaptive-sync"

        # Without this, gamescope will automatically attempt to scale
        # windows that are rendered at lower resolutions.
        #
        # This is particularly annoying for launchers.
        "--max-scale 1"

        # Ensure that games continue rendering, albeit at a low
        # framerate, even when unfocused. This is required due to
        # xwayland bugs; many games will lose network connection and
        # such if they don't tick regularly.
        "--nested-unfocused-refresh 30"

        # --steam is also an option (allows steam to set options at
        # runtime), but this currently breaks rendering
      ];
    };
  };
}
