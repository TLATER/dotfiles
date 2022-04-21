{
  self,
  mkTest,
  alejandra,
}:
mkTest {
  name = "lint-alejandra";
  src = self;
  checkInputs = [alejandra];
  checkPhase = ''
    mkdir -p $out
    alejandra --check \
        --exclude ./dotfiles/emacs.d/share/yatemplate/ \
        --exclude ./nixpkgs/pkgs/_sources/ \
        . \
    | tee $out/test.log
  '';
}
