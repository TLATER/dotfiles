{ pkgs, ... }:
{
  environment.systemPackages = [ pkgs.mangohud ];

  # Make steam controller work
  hardware.steam-hardware.enable = true;
  services.joycond.enable = true;

  programs = {
    steam = {
      enable = true;
      extraCompatPackages = [ pkgs.proton-ge-bin ];
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
