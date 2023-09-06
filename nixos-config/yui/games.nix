{
  pkgs,
  flake-inputs,
  ...
}: {
  imports = [
    flake-inputs.nix-gaming.nixosModules.default
  ];

  environment.systemPackages = [pkgs.mangohud];

  # Appears to resolve issues with crackling audio under high load
  services.pipewire.lowLatency.enable = true;

  programs.steam.enable = true;

  # Make steam controller work
  hardware.steam-hardware.enable = true;
  services.joycond.enable = true;

  # This sets up gamescope as an independent compositor through a
  # .desktop file.
  #
  # TODO(tlater): Figure out how to make this actually usable from
  # greetd.
  programs.gamescope = {
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

  # Star citizen needs more
  boot.kernel.sysctl."vm.max_map_count" = 16777216;
}
