{
  lib,
  rustPlatform,

  localLib,
  cargo,
}:
let
  src = ../../home-config/dotfiles/eww/desktop-logic;
  cargoToml = lib.importTOML (src + /Cargo.toml);
in
rustPlatform.buildRustPackage {
  inherit src;
  inherit (cargoToml.package) version;
  pname = cargoToml.package.name;
  cargoLock.lockFile = src + /Cargo.lock;

  meta.mainProgram = "desktop-logic";
  passthru.updateScript = localLib.writeUpdateScript {
    packageToUpdate = "desktop-logic";

    utils = [ cargo ];

    script = ''
      cd home-config/dotfiles/eww/desktop-logic
      cargo update
    '';
  };
}
