{ lib, ... }:
{
  /**
    A helper for creating btrfs subvolumes with disko.

    # Inputs

    `name` (String)
    : The name/path of the btrfs subvolume

    `options` (Attrset | String)
    : The disko subvolume options, or a string.

      If an attrset is given, these are returned transparently, and
      will be used by disko as the subvolume configuration.

      If a string is given, it is used as the mountpoint, and a set of
      standard mount options are set.
  */
  mapSubvolumes = lib.mapAttrs (
    _name: options:
    if (lib.types.attrs.check options) then
      options
    else
      {
        mountpoint = options;
        mountOptions = [
          "compress=zstd"
          "noatime"
        ];
      }
  );

  /**
    Specifies the system for all (legacy)?[Pp]ackages in a flake input attrset.

    # Inputs

    `inputs` (Attrset)
    : A flake input attrset.

    `system`  (String)
    : The system that should be used for all inputs.
  */
  flattenFlakeInputs =
    inputs: system:
    lib.mapAttrs (
      _:

      lib.mapAttrs (
        name: output:
        if
          (lib.elem name [
            "checks"
            "devShells"
            "packages"
            "pkgs-lib"
            "legacyPackages"
          ])
        then
          output.${system}
        else
          output
      )
    ) inputs;
}
