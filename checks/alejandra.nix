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
          --exclude ./home-config/dotfiles/emacs.d/share/templates/ \
          --exclude ./pkgs/_sources/ \
          --exclude ./nixos-config/ct-lt-02052/hardware-configuration.nix \
          --exclude ./nixos-config/yui/hardware-configuration.nix \
          . \
      | tee $out/test.log
    '';
  }
