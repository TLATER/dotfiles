{ melpaBuild, fetchFromGitHub }:
melpaBuild {
  src = fetchFromGitHub {
    owner = "nemethf";
    repo = "eglot-x";
    rev = "60cf3dcc02f268c8f561dc47998f01e4884ab9a8";
    hash = "sha256-FknyiRwGt9nukFhYgMHeruOFEeK9mmV9ZO6Pe9n6vas=";
  };

  version = "20260120.1716";
  pname = "eglot-x";
}
