{
  self,
  sources,
  hostPlatform,
  emacsPackagesFor,
  emacsMacport,
  emacs29,
  runCommandLocal,
}:
let
  emacsPlatform = if hostPlatform.isDarwin then emacsMacport else emacs29;

  overrides = self: _super: { eglot-x = self.callPackage ./eglot-x.nix { inherit sources; }; };

  emacsPkgs = (emacsPackagesFor emacsPlatform).overrideScope overrides;

  # Compute the list of leaf-d packages.
  package-list = runCommandLocal "package-list" { buildInputs = [ emacsPkgs.emacs ]; } ''
    HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
          -L ${emacsPkgs.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -l ${./leaf-package-list.el} \
          --eval "(leaf-package-list \"${self}/home-config/dotfiles/emacs.d/init.el\")" \
          > $out
  '';

  required-packages = builtins.fromJSON (builtins.readFile package-list) ++ [ "leaf" ];

  custom-emacs = emacsPkgs.emacs.pkgs.withPackages (
    epkgs:
    (map (package: builtins.getAttr package epkgs) required-packages)
    ++ [
      (epkgs.treesit-grammars.with-grammars (
        grammars: with grammars; [
          tree-sitter-bash
          tree-sitter-css
          tree-sitter-typescript
          tree-sitter-tsx
        ]
      ))
    ]
  );
in
custom-emacs
