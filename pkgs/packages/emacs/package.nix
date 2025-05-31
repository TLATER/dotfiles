{
  pkgs,
  flake-inputs,
  sources,
  lib,
  hostPlatform,
  emacsMacport,
  emacs30-pgtk,
}:
let
  package = if hostPlatform.isDarwin then emacsMacport else emacs30-pgtk;
  emacsDotfiles = "${flake-inputs.self}/home-config/dotfiles/emacs.d";

  emacsWithPackagesFromUsePackage = import "${flake-inputs.emacs-overlay}/elisp.nix" {
    inherit pkgs;
  };

  # Compute the list of leaf-d packages.
  emacsConfigs =
    [ "${emacsDotfiles}/init.el" ]
    ++ (lib.mapAttrsToList (fname: _: "${emacsDotfiles}/config/${fname}") (
      lib.filterAttrs (_: type: type == "regular") (builtins.readDir "${emacsDotfiles}/config/")
    ));
in
emacsWithPackagesFromUsePackage {
  inherit package;

  # TODO(tlater): Consider adding multi-file support upstream.
  config = lib.concatMapStringsSep "\n" builtins.readFile emacsConfigs;
  extraEmacsPackages = epkgs: [
    (epkgs.treesit-grammars.with-grammars (
      grammars: with grammars; [
        tree-sitter-bash
        tree-sitter-css
        tree-sitter-dockerfile
        tree-sitter-json
        tree-sitter-nix
        tree-sitter-nu
        tree-sitter-python
        tree-sitter-rust
        tree-sitter-typescript
        tree-sitter-tsx
        tree-sitter-yaml
      ]
    ))
  ];
  override = self: _super: {
    eglot-x = self.callPackage ./eglot-x.nix { inherit sources; };
    kdl-mode = self.callPackage ./kdl-mode.nix { inherit sources; };
  };
}
