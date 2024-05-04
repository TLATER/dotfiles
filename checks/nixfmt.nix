{
  self,
  mkTest,
  lib,
  alejandra,
  generatedFiles,
}:
let
  inherit (lib) concatStringsSep;
  inherit (lib.sources) sourceFilesBySuffices;
  excludes = concatStringsSep " " (map (f: "-path ./${f}") generatedFiles);
in
mkTest {
  name = "lint-nixfmt";
  src = sourceFilesBySuffices self [ ".nix" ];
  checkInputs = [ alejandra ];
  checkPhase = ''
    mkdir -p $out
    find . -name '*.nix' ! ${excludes} -exec nixfmt --check {} + | tee $out/test.log
  '';
}
