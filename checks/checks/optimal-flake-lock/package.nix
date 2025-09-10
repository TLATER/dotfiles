{
  self,
  lib,
  mkTest,

  nushell,
}:
mkTest {
  name = "lints";

  # We *could* define the regexes for the files to include in our
  # `src` here, which would make nix only run the checks if any
  # relevant files have been changed.
  #
  # The runtime is already super tiny, though, so skipping the build
  # is almost pointless, and it's not like we'll ever not touch a nix
  # file.
  #
  # *Maybe* if the repo grows an order of magnitude or two and we
  # split apart the nix and shell checks again.
  src = lib.cleanSourceWith {
    src = self;
    filter = lib.cleanSourceFilter;
  };

  checkInputs = [ nushell ];

  checkPhase = ''
    nu ${./optimal-flake-lock.nu}
  '';
}
