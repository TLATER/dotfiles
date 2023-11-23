{
  self,
  lib,
  mkTest,
  statix,
  generatedFiles,
}: let
  inherit (lib) concatStringsSep sourceFilesBySuffices;
  excludes = "--ignore ${concatStringsSep " " generatedFiles}";
in
  mkTest {
    name = "lint-statix";
    src = sourceFilesBySuffices self [".nix"];
    checkInputs = [statix];
    checkPhase = ''
      mkdir -p $out
      statix check ${excludes} | tee $out/test.log
    '';
  }
