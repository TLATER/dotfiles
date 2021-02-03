{ pkgs }:

pkgs.lispPackages.stumpwm.overrideAttrs (oldAttrs: rec {
  propagatedBuildInputs = with pkgs;
    [ lispPackages.clx-truetype lispPackages.xembed ]
    ++ (oldAttrs.propagatedBuildInputs or [ ]);
})
