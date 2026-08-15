{
  inputs,

  self',
  ast-grep,
  nushellPlugins,
  nix-prefetch-github,
}:
let
  inherit (inputs.self.nixosConfigurations.yui.config.boot) kernelPackages;
in
(kernelPackages.nvidiaPackages.mkDriver {
  version = "595.91.07";
  sha256_64bit = "sha256-yiPIjdJLB6GRZE4eEc+3vN11NzBXSa9A+YABiwleYxM=";
  openSha256 = "sha256-OB8Epd+qn/WywxsPiFpxEOAzlJqb6I1SyRoV3a8l71k=";

  useSettings = false;
  usePersistenced = false;
}).overrideAttrs
  (pkg: {
    passthru = pkg.passthru // {
      updateScript = self'.builders.writeUpdateScript {
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
