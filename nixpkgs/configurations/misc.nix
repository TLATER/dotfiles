{ config, ... }:

{
  xdg.userDirs = {
    enable = true;
    desktop = "${config.home.homeDirectory}"; # Work around firefox creating a "Desktop" directory
  };
}
