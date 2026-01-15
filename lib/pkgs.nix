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
        "--plugins [${lib.concatStringsSep " " (map lib.getExe plugins)}]"
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
}
