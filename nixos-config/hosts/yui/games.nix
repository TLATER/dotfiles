{
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    # flake-inputs.nix-gaming.nixosModules.pipewireLowLatency
    flake-inputs.aagl.nixosModules.default
  ];

  environment.systemPackages = [pkgs.mangohud];

  # Appears to resolve issues with crackling audio under high load
  # TODO(tlater): Borked due to recent nixpkgs patch: https://github.com/fufexan/nix-gaming/issues/161
  # services.pipewire.lowLatency.enable = true;

  # Make steam controller work
  hardware.steam-hardware.enable = true;
  services.joycond.enable = true;

  programs = {
    steam.enable = true;
    anime-game-launcher.enable = true;
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
  };

  # Star citizen needs more
  boot.kernel.sysctl."vm.max_map_count" = 16777216;

  # AAGL
  nix.settings = flake-inputs.aagl.nixConfig;
}
