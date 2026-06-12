/**
  Yui-specific application configuration.
*/
{ inputs, pkgs, ... }:
let
  pkgsGames = inputs.nix-gaming.packages.${pkgs.stdenv.hostPlatform.system};
  pkgsSelf = inputs.self.packages.${pkgs.stdenv.hostPlatform.system};

  osuLazer = pkgsGames.osu-lazer-bin.override { gmrun_enable = false; };
in
{
  services.flatpak = {
    update.auto.enable = false;
    uninstallUnmanaged = false;

    packages = [
      "de.schmidhuberj.tubefeeder"
      "com.github.rafostar.Clapper"
    ];
  };

  environment.systemPackages = [
    osuLazer

    pkgs.prismlauncher

    pkgsSelf.edopro
    pkgsSelf.jazz-jackrabbit-2
  ];
}
