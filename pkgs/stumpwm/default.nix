{
  sources,
  writeShellApplication,
  lispPackages_new,
  sbcl,
}: let
  inherit (lispPackages_new) sbclWithPackages;

  stumpwm = lispPackages_new.build-asdf-system {
    inherit (sources.stumpwm) pname version src;
    lisp = "${sbcl}/bin/sbcl --script";

    systems = [
      "stumpwm"
      "dynamic-mixins"
    ];

    lispLibs = with lispPackages_new.sbclPackages; [
      alexandria
      cl-ppcre
      clx
    ];
  };

  sbclWithStumpwm = sbclWithPackages (sbclPkgs:
    with sbclPkgs; [
      stumpwm
      clx-truetype
      xembed
    ]);
in
  writeShellApplication {
    name = "stumpwm";

    runtimeInputs = [sbclWithStumpwm];

    text = ''
      sbcl \
        --non-interactive \
        --eval '(require :asdf)' \
        --eval '(asdf:load-system :stumpwm)' \
        --eval '(stumpwm:stumpwm)'
    '';
  }
