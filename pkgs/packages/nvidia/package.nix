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
  version = "580.126.09";
  sha256_64bit = "sha256-TKxT5I+K3/Zh1HyHiO0kBZokjJ/YCYzq/QiKSYmG7CY=";
  openSha256 = "sha256-ychsaurbQ2KNFr/SAprKI2tlvAigoKoFU1H7+SaxSrY=";

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
