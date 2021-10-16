{ sources, stdenv, hostPlatform, emacsPackagesNgGen, emacsMacport, emacs
, runCommand, runCommandLocal, fetchurl, lzip }:

let
  use-package-list = stdenv.mkDerivation rec {
    inherit (sources.bauer) src version;
    pname = "use-package-list";
    installPhase = ''
      mkdir -p $out/
      cp site-lisp/use-package-list.el $out/
    '';
  };

  emacsDistribution = (emacsPackagesNgGen
    (if hostPlatform.isDarwin then emacsMacport else emacs));

  # Compute the list of use-package-d packages.
  package-list =
    runCommand "package-list" { buildInputs = [ emacsDistribution.emacs ]; } ''
      HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
            -L ${emacsDistribution.use-package}/share/emacs/site-lisp/elpa/use-package-* \
            -L ${emacsDistribution.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
            -l ${use-package-list}/use-package-list.el \
            --eval "(use-package-list \"${../../dotfiles/emacs.d}/init.el\")" \
            > $out
    '';

  required-packages = builtins.fromJSON (builtins.readFile package-list)
    ++ [ "use-package" ];

in emacsDistribution.emacsWithPackages
(epkgs: map (required: builtins.getAttr required epkgs) required-packages)
