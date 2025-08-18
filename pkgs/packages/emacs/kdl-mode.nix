# TODO(tlater): Should be available upstream starting with 25.11
{ melpaBuild, fetchFromGitHub }:
melpaBuild (drv: {
  src = fetchFromGitHub {
    owner = "taquangtrung";
    repo = "emacs-kdl-mode";
    rev = "2d849e298199f490e4894c01764a8a83decd704a";
    hash = "sha256-5PHAV1yrpiZrWJDI5r9dSgfbSJQJ80nQXvklr24saqM=";
  };

  version = "20250620.830";
  pname = "kdl-mode";
})
