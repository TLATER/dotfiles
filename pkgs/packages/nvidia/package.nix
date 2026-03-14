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
  version = "580.142";
  sha256_64bit = "sha256-IJFfzz/+icNVDPk7YKBKKFRTFQ2S4kaOGRGkNiBEdWM=";
  openSha256 = "sha256-v968LbRqy8jB9+yHy9ceP2TDdgyqfDQ6P41NsCoM2AY=";

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
