{
  flake-inputs,
  pkgs,
  config,
  ...
}:
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

      # TODO(tlater): Star Citizen doesn't currently work due to EAC
      # not liking the wine version we're using (10.0+).
      #
      # Since this is required for ntsync to work, guess we'll have to
      # wait for EAC to "fix itself", as the LUG installer comments
      # put it.
      starCitizen = pkgsGames.star-citizen.override {
        inherit (pkgsUnstable) proton-ge-bin;

        useUmu = true;
        location = "$HOME/.local/share/wine-prefixes/star-citizen";

        gameScopeEnable = true;
        gameScopeArgs = config.programs.gamescope.args;
      };
    in
    [
      osuLazer
      starCitizen
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
    };

    wine = {
      enable = true;
      package = pkgsUnstable.wine-staging;
      ntsync = true;
    };
  };
}
