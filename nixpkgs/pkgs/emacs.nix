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

  # Override some elpa sources - see
  # https://github.com/NixOS/nixpkgs/issues/110796
  emacsOverrides = self: super: rec {
    spinner = super.spinner.override {
      elpaBuild = args:
        super.elpaBuild (args // {
          buildInputs = [ lzip ];
          src = sources.elpa-spinner.src;
        });
    };
    project = super.project.override {
      elpaBuild = args:
        super.elpaBuild (args // { src = sources.elpa-project.src; });
    };
  };

  fixedEmacsDistribution = emacsDistribution.overrideScope' emacsOverrides;

  # Compute the list of use-package-d packages.
  package-list = runCommand "package-list" {
    buildInputs = [ fixedEmacsDistribution.emacs ];
  } ''
    HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
          -L ${fixedEmacsDistribution.use-package}/share/emacs/site-lisp/elpa/use-package-* \
          -L ${fixedEmacsDistribution.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -l ${use-package-list}/use-package-list.el \
          --eval "(use-package-list \"${../../dotfiles/emacs.d}/init.el\")" \
          > $out
  '';

  required-packages = builtins.fromJSON (builtins.readFile package-list)
    ++ [ "use-package" ];

in fixedEmacsDistribution.emacsWithPackages
(epkgs: map (required: builtins.getAttr required epkgs) required-packages)
