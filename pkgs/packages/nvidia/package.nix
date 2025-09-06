{
  flake-inputs,

  localLib,
  ast-grep,
  nushellPlugins,
  nix-prefetch-github,
}:
let
  inherit (flake-inputs.self.nixosConfigurations.yui.config.boot) kernelPackages;
in
(kernelPackages.nvidiaPackages.mkDriver {
  version = "580.82.07";
  sha256_64bit = "sha256-Bh5I4R/lUiMglYEdCxzqm3GLolQNYFB0/yJ/zgYoeYw=";
  openSha256 = "sha256-8/7ZrcwBMgrBtxebYtCcH5A51u3lAxXTCY00LElZz08=";

  useSettings = false;
  usePersistenced = false;
}).overrideAttrs
  (pkg: {
    passthru = pkg.passthru // {
      updateScript = localLib.writeUpdateScript {
        packageToUpdate = "nvidia";

        utils = [
          ast-grep
          nix-prefetch-github
        ];
        nushellPlugins = [ nushellPlugins.query ];

        script = ./update.nu;
      };
    };
  })
