{
  system,
  flake-inputs,
  makeRustPlatform,
}:
let
  # TODO(tlater): Switch to stable whenever the `btree_extract_if`
  # feature lands.
  #
  # See https://github.com/rust-lang/rust/issues/70530
  toolchain = flake-inputs.fenix.packages.${system}.minimal.toolchain;
  rustPlatform = makeRustPlatform {
    cargo = toolchain;
    rustc = toolchain;
  };
in
rustPlatform.buildRustPackage {
  pname = "";
  version = "";
  src = ../../home-config/dotfiles/eww/desktop-logic;
  cargoLock.lockFile = ../../home-config/dotfiles/eww/desktop-logic/Cargo.lock;
}
