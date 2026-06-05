{
  lib,
  writers,
  nushell,

  runCommand,
  makeBinaryWrapper,
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
      interpreter = lib.getExe (
        runCommand "wrapped-nu"
          {
            nativeBuildInputs = [ makeBinaryWrapper ];
            meta.mainProgram = "nu";
          }
          ''
            makeBinaryWrapper ${lib.getExe nushell} $out/bin/nu \
              --add-flag --plugins \
              --add-flag '[${lib.concatStringsSep " " (map lib.getExe plugins)}]' \
              --add-flag --no-config-file
          ''
      );

      makeWrapperArgs =
        (lib.optionals (packages != [ ]) [
          "--prefix"
          "PATH"
          ":"
          (lib.makeBinPath packages)
        ])
        ++ extraMakeWrapperArgs;
    };

  writeNuBinWith = args: name: writeNuWith args "/bin/${name}";
}
