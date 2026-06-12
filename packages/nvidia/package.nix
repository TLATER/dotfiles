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
  version = "595.80";
  sha256_64bit = "sha256-PVTIP+B/01c/8M66hXTAYTLg9T2Hy9u1gq43K7TF1Hg=";
  openSha256 = "sha256-nonwYYPItHeMC/5Ox/TlWhjiddMPu4PLqNhgIg+bfW8=";

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
