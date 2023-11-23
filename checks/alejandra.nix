{
  self,
  mkTest,
  lib,
  alejandra,
  generatedFiles,
}: let
  inherit (lib) concatStringsSep;
  inherit (lib.sources) sourceFilesBySuffices;
  excludes = concatStringsSep " " (map (f: "--exclude ./${f}") generatedFiles);
in
  mkTest {
    name = "lint-alejandra";
    src = sourceFilesBySuffices self [".nix"];
    checkInputs = [alejandra];
    checkPhase = ''
      mkdir -p $out
      echo alejandra --check ${excludes} . | tee $out/test.log
      alejandra --check ${excludes} . | tee $out/test.log
    '';
  }
