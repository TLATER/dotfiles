{ stdenv, fetchgit, hostPlatform, emacsPackagesNgGen, emacsMacport, emacs
, runCommand }:

let
  use-package-list = stdenv.mkDerivation rec {
    pname = "use-package-list";
    version = "v1.5.3";
    src = fetchgit {
      url = "https://github.com/matthewbauer/bauer.git";
      rev = version;
      sha256 = "IBzX7WASluoxXVdWkoJHRIQQF4Fi7IJOvRfRl+W3YZI=";
      fetchSubmodules = false;
    };
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
