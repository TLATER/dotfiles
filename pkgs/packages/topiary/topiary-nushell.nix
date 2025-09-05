{ runCommand, fetchFromGitHub }:
runCommand "topiary-nushell"
  {
    version = "0-unstable-2025-08-23";

    src = fetchFromGitHub {
      owner = "blindFS";
      repo = "topiary-nushell";
      rev = "48934dd3086a2f51ee60b973bb523874ead6af36";
      hash = "sha256-pxgG2zYWLrxksDIs/nQtnpaITLYhYZ5LktWqiH/Zs1w=";
    };

    # Don't update this package for now; it's tied pretty heavily to
    # specific grammar versions.
    #
    # passthru.updateScript = localLib.nixUpdateScript {
    #   packageToUpdate = "topiary-nushell";
    #   version = "branch";
    # };
  }
  ''
    install -D --target-directory $out/share/queries/ $src/languages/nu.scm
  ''
