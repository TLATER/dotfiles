{ writers, python3Packages, ... }:
writers.writePython3Bin "update-nix-sources" {
  libraries = [
    python3Packages.pygithub
    python3Packages.tree-sitter
    python3Packages.tree-sitter-grammars.tree-sitter-nix
  ];
} (builtins.readFile ./update-nix-sources.py)
