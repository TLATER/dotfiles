{ pkgs, ... }:
{
  imports = [
    # flake-inputs.nix-gaming.nixosModules.pipewireLowLatency
  ];

  environment.systemPackages = [ pkgs.mangohud ];

  # Appears to resolve issues with crackling audio under high load
  # TODO(tlater): Borked due to recent nixpkgs patch: https://github.com/fufexan/nix-gaming/issues/161
  # services.pipewire.lowLatency.enable = true;

  # Make steam controller work
  hardware.steam-hardware.enable = true;
  services.joycond.enable = true;

  programs = {
    steam = {
      enable = true;
      gamescopeSession.enable = true;
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
  };

  # Star citizen needs more
  boot.kernel.sysctl."vm.max_map_count" = 16777216;
}
