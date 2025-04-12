{
  self,
  mkTest,
  lib,
  generatedFiles,
  deadnix,
}:
let
  inherit (lib) concatStringsSep;
  inherit (lib.sources) sourceFilesBySuffices;

  excludes = "--exclude ${concatStringsSep " " (map (f: "./${f}") generatedFiles)}";
in
mkTest {
  name = "lint-deadnix";
  src = sourceFilesBySuffices self [ ".nix" ];
  checkInputs = [ deadnix ];
  checkPhase = ''
    mkdir -p $out
    deadnix --fail ${excludes} | tee $out/test.log
  '';
}
