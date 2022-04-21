{
  self,
  mkTest,
  fetchpatch,
  lib,
  local,
  emacsPackages,
  git,
  python3,
}: let
  inherit (local) emacs;
  inherit (lib.sources) sourceFilesBySuffices;

  elisp-lint-pkg = emacsPackages.elisp-lint.overrideAttrs (_old: {
    patches = [
      # Patch to allow long lines if they contain URLs
      (fetchpatch {
        name = "long-urls.patch";
        url = "https://patch-diff.githubusercontent.com/raw/gonewest818/elisp-lint/pull/29.patch";
        sha256 = "sha256-mzJfHtT5Pe8jWni4rVYdGvigP/emGGDVi0ZAVYMkNq0=";
      })
    ];
  });

  package-lint = "${emacsPackages.package-lint}/share/emacs/site-lisp/elpa/package-lint-*";
  elisp-lint = "${elisp-lint-pkg}/share/emacs/site-lisp/elpa/elisp-lint-*";
in
  mkTest {
    name = "lint-emacs";

    src = sourceFilesBySuffices self [".el"];

    checkInputs = [emacs git python3];
    checkPhase = ''
      mkdir -p $out

      export SCANNING_PACKAGES=true
      find "$PWD/dotfiles/emacs.d/" \
          -path "$PWD/dotfiles/emacs.d/share" -prune \
          -o -type f -exec \
          emacs --batch -Q \
              -L ${package-lint} \
              -L ${elisp-lint} \
              --eval="(require 'elisp-lint)" \
              -f elisp-lint-files-batch \
              --no-package-lint \
              {} + \
      | tee $out/test.log

      # Also fail on errors in use-package macros (these are only
      # reported using `(message)`, and don't show up in the compilation
      # log buffer that elisp-lint checks).
      ! grep '^Error ' $out/test.log
    '';
  }
