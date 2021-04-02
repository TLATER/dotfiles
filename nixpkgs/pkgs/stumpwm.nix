{ lispPackages }:

lispPackages.stumpwm.overrideAttrs (oldAttrs: rec {
  propagatedBuildInputs = [ lispPackages.clx-truetype lispPackages.xembed ]
    ++ (oldAttrs.propagatedBuildInputs or [ ]);
})
