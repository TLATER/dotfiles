{ runCommand, fetchFromGitHub }:
runCommand "topiary-nushell"
  {
    pname = "topiary-nushell";
    version = "0-unstable-2026-03-04";

    src = fetchFromGitHub {
      owner = "blindFS";
      repo = "topiary-nushell";
      rev = "6e2f9b339a664a46e4015fa5d79e537807fefa39";
      hash = "sha256-fTfxSnVI7TY6vQhD+GimPBRJ4K0SyyVtoLcLGH3xIPc=";
    };
  }
  ''
    install -D --target-directory $out/share/queries/ $src/queries/nu.scm
  ''
