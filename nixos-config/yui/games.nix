{
  # Make steam controller work
  hardware.steam-hardware.enable = true;
  services.joycond.enable = true;

  programs.steam.enable = true;
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
