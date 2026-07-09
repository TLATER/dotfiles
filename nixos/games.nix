/**
  Configuration for hosts that run video games.
*/
{ inputs, pkgs, ... }: {
  imports = [
    inputs.nix-gaming.nixosModules.pipewireLowLatency
    inputs.nix-gaming.nixosModules.platformOptimizations
    inputs.nix-gaming.nixosModules.wine
  ];

  hardware.steam-hardware.enable = true;

  services = {
    joycond.enable = true;
    pipewire.lowLatency.enable = true;
  };

  programs = {
    steam = {
      enable = true;
      extraPackages = [ pkgs.hidapi ];

      extraCompatPackages = [ pkgs.proton-ge-bin ];
      platformOptimizations.enable = true;
      protontricks.enable = true;
    };

    wine = {
      enable = true;
      package = pkgs.wine-staging;
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
