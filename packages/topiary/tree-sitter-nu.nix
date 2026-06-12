{ tree-sitter-grammars, fetchFromGitHub }:
tree-sitter-grammars.tree-sitter-nu.overrideAttrs {
  src = fetchFromGitHub {
    owner = "nushell";
    repo = "tree-sitter-nu";
    rev = "f4793e3809bb84e78dee260b47085d8203a58d88";
    hash = "sha256-0tQOALi8079pqy12mGG3eqsqv2FsqVvRnetp4xXKH9s=";
  };
}
