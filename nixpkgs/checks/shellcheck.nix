{
  self,
  stdenv,
  file,
  shellcheck,
}:
stdenv.mkDerivation {
  name = "lint-shellcheck";
  src = self;
  dontInstall = true;
  doCheck = true;
  checkInputs = [file shellcheck];
  checkPhase = ''
    mkdir -p $out
    shellcheck $(file dotfiles/bin/* \
                 | grep -e 'Bourne-Again shell script' -e 'a /usr/bin/env sh script' \
                 | cut -d ':' -f 1) \
    | tee $out/test.log
    shellcheck dotfiles/zshenv dotfiles/zsh/.zshrc dotfiles/zsh/*.zsh --shell=bash \
    | tee -a $out/test.log
  '';
}
