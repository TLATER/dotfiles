{
  self,
  sources,
  stdenv,
  hostPlatform,
  emacsPackagesFor,
  emacsMacport,
  emacs29,
  runCommandLocal,
  git,
}:
let
  emacsPlatform = if hostPlatform.isDarwin then emacsMacport else emacs29;

  use-package-list = stdenv.mkDerivation {
    inherit (sources.bauer) src version;
    pname = "use-package-list";
    installPhase = ''
      mkdir -p $out/
      cp site-lisp/use-package-list.el $out/
    '';
  };

  overrides = self: _super: { eglot-x = self.callPackage ./eglot-x.nix { inherit sources; }; };

  emacsPkgs = (emacsPackagesFor emacsPlatform).overrideScope overrides;

  # Compute the list of use-package-d packages.
  package-list = runCommandLocal "package-list" { buildInputs = [ emacsPkgs.emacs ]; } ''
    HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
          -L ${emacsPkgs.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -l ${use-package-list}/use-package-list.el \
          --eval "(use-package-list \"${self}/home-config/dotfiles/emacs.d/init.el\")" \
          > $out
  '';

  required-packages = builtins.fromJSON (builtins.readFile package-list);

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
            --eval "(setq byte-compile-error-on-warn t)" \
            -f batch-byte-compile \
            "$out/init.el" "$out/config/"*
      '';
in
custom-emacs // (custom-emacs.passthru // { dotfiles = compiled-dotfiles; })
