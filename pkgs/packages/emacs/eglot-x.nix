{ melpaBuild, fetchFromGitHub }:
melpaBuild {
  src = fetchFromGitHub {
    owner = "nemethf";
    repo = "eglot-x";
    rev = "b92c44e6b34f8df0539d3c8ab5992c5a7eb815d5";
    hash = "sha256-VvamDqZ3NowM6XfRlC2exsM6ssRBqWUw6ziKgqdphwM=";
  };

  version = "20251219.1330";
  pname = "eglot-x";
}
