{
  self,
  mkTest,
  lib,
  alejandra,
}: let
  inherit (lib.sources) sourceFilesBySuffices;
in
  mkTest {
    name = "lint-alejandra";
    src = sourceFilesBySuffices self [".nix"];
    checkInputs = [alejandra];
    checkPhase = ''
      mkdir -p $out
      alejandra --check \
          --exclude ./dotfiles/emacs.d/share/templates/ \
          --exclude ./nixpkgs/pkgs/_sources/ \
          . \
      | tee $out/test.log
    '';
  }
