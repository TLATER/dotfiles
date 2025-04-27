{
  pkgs,
  lib,

  flake-inputs,
  sources,
}:
lib.makeScope (extra: lib.callPackageWith (pkgs // extra // { inherit flake-inputs sources; }))
  (self: {
    luacxx = self.callPackage ./luacxx.nix { };
    irrlicht-edopro = self.callPackage ./irrlicht.nix { };
    ocgcore = self.callPackage ./ocgcore.nix { };
    edopro-unwrapped = self.callPackage ./edopro-unwrapped.nix { };

    edopro = self.callPackage ./edopro.nix { };
  })
