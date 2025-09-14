{
  lib,
  writers,
  nushell,
  ...
}:
rec {
  writeNuWith =
    {
      packages ? [ ],
      plugins ? [ ],
      extraMakeWrapperArgs ? [ ],
    }:
    writers.makeScriptWriter {
      interpreter = lib.concatStringsSep " " [
        (lib.getExe nushell)
        "--no-config-file"
        "--plugins [${lib.concatStringsSep " " (map (p: lib.getExe p) plugins)}]"
      ];

      makeWrapperArgs = [
        "--prefix"
        "PATH"
        ":"
        (lib.makeBinPath packages)
      ]
      ++ extraMakeWrapperArgs;
    };

  writeNuBinWith = args: name: writeNuWith args "/bin/${name}";

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
  mapSubvolumes =
    subvolumes:
    lib.mapAttrs (
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
    ) subvolumes;
}
