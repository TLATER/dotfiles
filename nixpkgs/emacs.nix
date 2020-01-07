{ pkgs, ensure ? (f: n: f), basenameOf ? (f: n: f) }:

with pkgs; with emacsPackagesNg;
  let emacsDistribution = (emacsPackagesNgGen (if hostPlatform.isDarwin then pkgs.emacsMacport else pkgs.emacs));
      # Include the emacs config directory; we want to include what
      # use-package includes, so we need to be able to read this.
      config = builtins.filterSource
        (path: type: type != "directory"  || basenameOf path != "*.elc")
        ../dotfiles/emacs.d;

      # Compute the list of use-package-d packages.
      package-list =
        runCommand "package-list" {
          buildInputs = [ emacsDistribution.emacs ];
        } ''
        HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
              -L ${emacsDistribution.use-package}/share/emacs/site-lisp/elpa/use-package-* \
              -L ${emacsDistribution.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
              -l ${ensure ./use-package-list.el "site-lisp/use-package-list.el"} \
              --eval "(use-package-list \"${config}/init.el\")" \
              > $out
        '';

      required-packages = builtins.fromJSON (builtins.readFile package-list) ++ ["use-package"];

  in emacsDistribution.emacsWithPackages(epkgs:
    map (required: builtins.getAttr required epkgs) required-packages)
