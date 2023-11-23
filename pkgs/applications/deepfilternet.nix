{
  lib,
  sources,
  system,
  flake-inputs,
  makeRustPlatform,
  symlinkJoin,
  pkg-config,
  fetchurl,
  alsa-lib,
  python3,
  hdf5,
}: let
  # Need to use unstable rust platform until this is resolved:
  # https://github.com/rust-lang/rust/issues/63292
  rustPlatform = let
    inherit (flake-inputs.fenix.packages.${system}.minimal) toolchain;
  in
    makeRustPlatform {
      cargo = toolchain;
      rustc = toolchain;
    };

  hdf5-12 = hdf5.overrideAttrs (_old: {
    version = "1.12.2";

    src = fetchurl {
      url = "https://support.hdfgroup.org/ftp/HDF5/releases/hdf5-1.12/hdf5-1.12.2/src/hdf5-1.12.2.tar.bz2";
      sha256 = "sha256-Goi742ITos6gyDlyAaRZZD5xVcnckeBiZ1s/sH7jiv4=";
    };

    postFixup = ""; # The 1.14.0 derivation needs to patch some additional things
  });
in
  rustPlatform.buildRustPackage {
    inherit (sources.deepfilternet) pname version src;

    buildInputs = [
      alsa-lib
    ];

    nativeBuildInputs = [
      pkg-config
      python3
    ];

    HDF5_DIR = symlinkJoin {
      name = "hdf5";
      paths = [hdf5-12 hdf5-12.dev];
    };

    checkFlags = [
      "--skip=reexport_dataset_modules::dataset::tests::test_hdf5_slice"
    ];

    cargoHash = lib.fakeHash;
    cargoBuildFlags = ["--package deep-filter-ladspa"];
    cargoLock = {
      lockFile = "${sources.deepfilternet.src}/Cargo.lock";

      # Upstream uses a version from a github PR for the moment, we can
      # probably change this once it is merged.
      #
      # Incidentally, we should be able to remove the `lockFile`
      # property in general once this happens.
      outputHashes."hdf5-0.8.1" = "sha256-TQaIgu2ww/2HTaJC7q/lLWjTdSwIJ2G2RvO0WS5mfcM=";
    };
  }
