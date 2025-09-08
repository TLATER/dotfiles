{ pkgs, flake-inputs }:
let
  inherit (pkgs) lib;
  inherit (flake-inputs.self.pkgs-lib.${pkgs.system}) writeNuBinWith;
in
{
  nixUpdateScript =
    {
      packageToUpdate,
      version ? null,
    }:
    writeNuBinWith
      {
        packages = [
          pkgs.nix-update
          pkgs.nixfmt-rfc-style

        ];
      }
      "update-${packageToUpdate}"
      ''
        (nix-update
          --flake
          --format
          ${lib.concatStringsSep " " (lib.optional (version != null) "--version=${version}")}
          ${packageToUpdate})
      '';

  writeUpdateScript =
    {
      script,
      packageToUpdate,
      utils ? [ ],
      nushellPlugins ? [ ],
    }:
    writeNuBinWith {
      packages = utils;
      plugins = nushellPlugins;
    } "update-${packageToUpdate}" script;
}
