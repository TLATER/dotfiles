{
  pkgs,
  lib,
  fetchzip,

  symlinkJoin,

  stdenv,

  lua5_4,

  # Update script
  localLib,
  ast-grep,
  nix-update,
}:
let
  edopro-assets = fetchzip {
    url = "https://github.com/ProjectIgnis/edopro-assets/releases/download/${scope.edopro-unwrapped.version}/ProjectIgnis-EDOPro-${scope.edopro-unwrapped.version}-linux.tar.gz";
    hash = "sha256-cta4k6yxrdaFFfum0eshEzLODExBfA+oVPqcOpXG9uk=";
  };

  scope =
    lib.makeScope (extra: lib.callPackageWith (pkgs // extra // { inherit edopro-assets; }))
      (self: {
        # EDOPro assumes a version of lua that was built for C++, but
        # nixpkgs only builds the C variant.
        #
        # This is pretty standard lua packaging silliness, so we just go
        # ahead and build a lua for C++.
        luacxx = lua5_4.overrideAttrs (old: {
          makeFlags = old.makeFlags ++ [ "CC=${stdenv.cc.targetPrefix}c++" ];
        });

        # EDOPro uses what is effectively a hard fork of upstream
        # irrlicht. Luckily, the build system isn't changed in any way, so
        # the nixpkgs derivation largely still works.
        irrlicht-edopro = self.callPackage ./irrlicht.nix { };

        ocgcore = self.callPackage ./ocgcore.nix { };
        edopro-unwrapped = self.callPackage ./edopro-unwrapped.nix { };
        edopro-wrapper = self.callPackage ./edopro-wrapper.nix { };
      });

in
symlinkJoin {
  inherit (scope.edopro-unwrapped) version;
  pname = "edopro";

  paths = [
    scope.edopro-unwrapped
    scope.edopro-wrapper
  ];

  passthru = {
    inherit scope;

    updateScript = localLib.writeUpdateScript {
      packageToUpdate = "edopro";
      utils = [
        ast-grep
        nix-update
      ];
      script = ./update.nu;
    };
  };
}
