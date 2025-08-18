{ melpaBuild, fetchFromGitHub }:
melpaBuild (drv: {
  src = fetchFromGitHub {
    owner = "nemethf";
    repo = "eglot-x";
    rev = "8e872efd3d0b7779bde5b1e1d75c8e646a1f729f";
    hash = "sha256-a2qkitikqGZBXI4aVdn8c7P4HFwep9RPWkOVBbgQV2g=";
  };

  # Build a MELPA unstable version string - this is the date with no
  # separators followed by the hour/minute of the commit. We don't
  # have the latter so we set it to 0.
  version = builtins.replaceStrings [ "-" ] [ "" ] drv.src.date + ".0";
  pname = "emacs-eglot-x";
})
