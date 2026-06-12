{ melpaBuild, fetchFromGitHub }:
melpaBuild {
  src = fetchFromGitHub {
    owner = "nemethf";
    repo = "eglot-x";
    rev = "46bca93291727454dd92567e761a1e2ab5622590";
    hash = "sha256-c8NzzK7SOYYDB803Osp3TOymrmwC07+dcvbI4waAfco=";
  };

  version = "20260216.2139";
  pname = "eglot-x";
}
