{ melpaBuild, fetchFromGitHub }:
melpaBuild (drv: {
  src = fetchFromGitHub {
    owner = "taquangtrung";
    repo = "emacs-kdl-mode";
    rev = "2d849e298199f490e4894c01764a8a83decd704a";
    hash = "sha256-5PHAV1yrpiZrWJDI5r9dSgfbSJQJ80nQXvklr24saqM=";
  };

  # Build a MELPA unstable version string - this is the date with no
  # separators followed by the hour/minute of the commit. We don't
  # have the latter so we set it to 0.
  version = builtins.replaceStrings [ "-" ] [ "" ] drv.src.date + ".0";
  pname = "emacs-kdl-mode";
})
