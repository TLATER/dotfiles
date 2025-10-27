{
  lib,
  rustPlatform,

  localLib,
  cargo,
  pkg-config,
  libxkbcommon,

  mkShell,
  rust-analyzer,
  rustc,
  rustfmt,
  clippy,
}:
let
  src = lib.fileset.toSource {
    root = ./.;
    fileset = lib.fileset.fileFilter (
      file: file.hasExt "rs" || file.hasExt "toml" || file.hasExt "lock"
    ) ./.;
  };

  cargoToml = lib.importTOML (src + /Cargo.toml);
in
rustPlatform.buildRustPackage {
  inherit src;
  inherit (cargoToml.package) version;
  pname = cargoToml.package.name;
  cargoLock.lockFile = src + /Cargo.lock;

  nativeBuildInputs = [ pkg-config ];
  buildInputs = [ libxkbcommon ];

  meta.mainProgram = "desktop-logic";

  passthru = {
    devShell = mkShell {
      packages = [
        pkg-config
        libxkbcommon

        rust-analyzer
        rustc
        rustfmt
        cargo
        clippy
      ];
    };

    updateScript = localLib.writeUpdateScript {
      packageToUpdate = "desktop-logic";

      utils = [ cargo ];

      script = ''
        cd home-config/dotfiles/eww/desktop-logic
        cargo update
      '';
    };
  };
}
