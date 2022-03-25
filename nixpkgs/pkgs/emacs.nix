{
  sources,
  stdenv,
  hostPlatform,
  emacsPackagesFor,
  emacsMacport,
  emacs,
  runCommand,
  runCommandLocal,
  fetchurl,
  lzip,
}: let
  emacsPlatform =
    if hostPlatform.isDarwin
    then emacsMacport
    else emacs;
  use-package-list = stdenv.mkDerivation rec {
    inherit (sources.bauer) src version;
    pname = "use-package-list";
    installPhase = ''
      mkdir -p $out/
      cp site-lisp/use-package-list.el $out/
    '';
  };

  overrides = self: super: {
    project = super.project.overrideAttrs (old: {
      src = fetchurl {
        url = "https://elpa.gnu.org/packages/project-0.8.1.tar";
        sha256 = "sha256-M7BzLw2jO0LZRDsSLGLGmurnRUoO9Cr6hQxSGDHSUmA=";
      };
    });

    xref = super.xref.overrideAttrs (old: {
      src = fetchurl {
        url = "https://elpa.gnu.org/packages/xref-1.3.2.tar";
        sha256 = "sha256-7d8t72xS4qaoo/ro0MKWwM5sPF/LQgIAfAY000Skm68=";
      };
    });
  };

  emacsPkgs = (emacsPackagesFor emacsPlatform).overrideScope' overrides;

  # Compute the list of use-package-d packages.
  package-list = runCommand "package-list" {buildInputs = [emacsPkgs.emacs];} ''
    HOME=/tmp SCANNING_PACKAGES=true emacs --batch --quick \
          -L ${emacsPkgs.use-package}/share/emacs/site-lisp/elpa/use-package-* \
          -L ${emacsPkgs.bind-key}/share/emacs/site-lisp/elpa/bind-key-* \
          -l ${use-package-list}/use-package-list.el \
          --eval "(use-package-list \"${../../dotfiles/emacs.d}/init.el\")" \
          > $out
  '';

  required-packages =
    builtins.fromJSON (builtins.readFile package-list)
    ++ ["use-package"];
in
  emacsPkgs.emacs.pkgs.withPackages
  (epkgs: map (package: builtins.getAttr package epkgs) required-packages)
