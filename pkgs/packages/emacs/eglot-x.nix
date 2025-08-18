{ melpaBuild, fetchFromGitHub }:
melpaBuild {
  src = fetchFromGitHub {
    owner = "nemethf";
    repo = "eglot-x";
    rev = "8e872efd3d0b7779bde5b1e1d75c8e646a1f729f";
    hash = "sha256-a2qkitikqGZBXI4aVdn8c7P4HFwep9RPWkOVBbgQV2g=";
  };

  version = "20250626.1641";
  pname = "eglot-x";
}
