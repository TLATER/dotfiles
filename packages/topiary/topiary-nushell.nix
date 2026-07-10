{ runCommand, fetchFromGitHub }:
runCommand "topiary-nushell"
  {
    pname = "topiary-nushell";
    version = "0-unstable-2026-07-02";

    src = fetchFromGitHub {
      owner = "blindFS";
      repo = "topiary-nushell";
      rev = "b187defff76caaea7c95614047c1779a675df0f6";
      hash = "sha256-a9yWF75XPll2EYGE0LEDByFCcLUC+DmgfRToqTUNi60=";
    };
  }
  ''
    install -D --target-directory $out/share/queries/ $src/queries/nu.scm
  ''
