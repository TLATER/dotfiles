{
  self,
  stdenv,
  alejandra,
}:
stdenv.mkDerivation {
  name = "lint-alejandra";
  src = self;
  dontInstall = true;
  doCheck = true;
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
