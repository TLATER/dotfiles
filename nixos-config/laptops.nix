{ pkgs, ... }:
{
  services = {
    upower = {
      enable = true;
      noPollBatteries = true;
    };

    auto-cpufreq.enable = true;
  };

  powerManagement.enable = true;
  environment.systemPackages = [ pkgs.brightnessctl ];
}
