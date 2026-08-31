{ tree-sitter-grammars, fetchFromGitHub }:
tree-sitter-grammars.tree-sitter-nu.overrideAttrs {
  src = fetchFromGitHub {
    owner = "nushell";
    repo = "tree-sitter-nu";
    rev = "d694570aa26b53d0d642460a0430e8aa07dcbea0";
    hash = "sha256-eWHAcV8bPCnL9y4PtPn6cJRylGQ2KMxCUoUGwDVigkg=";
  };
}
