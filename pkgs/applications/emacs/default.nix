{
  self,
  sources,
  hostPlatform,
  emacsPackagesFor,
  emacsMacport,
  emacs29,
  runCommandLocal,
  git,
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
    epkgs: map (package: builtins.getAttr package epkgs) required-packages
  );

  compiled-dotfiles =
    runCommandLocal "compiled-init"
      {
        buildInputs = [
          git
          custom-emacs
        ];
      }
      ''
        cp -r '${self}/home-config/dotfiles/emacs.d/' "$out"
        chmod -R u+w "$out"

        HOME=/tmp emacs --batch \
            -L "$out" \
            --eval "(setq byte-compile-error-on-warn t)" \
            -f batch-byte-compile \
            "$out"/{init.el,config/*} 2>&1 | tee compilation-output

        if grep -q '^Error' compilation-output; then
            echo 'Byte compilation errors present; exiting'
            exit 1
        else
            echo 'Byte compilation successful'
        fi
      '';
in
custom-emacs // (custom-emacs.passthru // { dotfiles = compiled-dotfiles; })
