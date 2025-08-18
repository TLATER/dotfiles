{
  lib,
  writers,

  edopro-assets,
  edopro-unwrapped,

  wayland,
  egl-wayland,
  libxkbcommon,
}:
writers.writeNuBin "EDOPro" {
  makeWrapperArgs = [
    # EDOPro loads certain libraries at runtime using some crazy
    # internal logic, and doesn't link
    # against them.
    #
    # Luckily it respects LD_LIBRARY_PATH, but unfortunately that does
    # mean we need to use it.
    "--prefix"
    "LD_LIBRARY_PATH"
    ":"
    (lib.makeLibraryPath [
      wayland
      egl-wayland
      libxkbcommon
    ])
    "--prefix"
    "PATH"
    ":"
    (lib.makeBinPath [ edopro-unwrapped ])
    "--add-flag"
    edopro-assets
  ];
} ./edopro-wrapper.nu
