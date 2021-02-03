{ config, pkgs, ... }:

# A sample config for configurations that can't use a flake
{
  nixpkgs.overlays = [
    (final: prev: {
      local = import ./pkgs { pkgs = prev; };
      nur = import (builtins.fetchTarball
        "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          pkgs = prev;
        };
    })
  ];

  home.username = "tlater";
  home.homeDirectory = "/home/${config.home.username}";

  imports = [
    ./.
    ./configurations/tty-programs
    ./configurations/graphical-programs

    ./configurations/graphical-programs/games.nix
    # ./configurations/tty-programs/work.nix
  ];
}
