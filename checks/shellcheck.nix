{
  self,
  mkTest,
  lib,
  file,
  shellcheck,
}: let
  inherit (builtins) elem;
  inherit (lib.lists) any;
  inherit (lib.sources) sourceByRegex sourceFilesBySuffices cleanSourceWith trace;
  inherit (lib.strings) hasInfix hasSuffix;
in
  mkTest {
    name = "lint-shellcheck";
    src = cleanSourceWith {
      filter = path: type:
        type
        == "directory"
        || hasInfix "dotfiles/bin" path
        || hasInfix "dotfiles/zsh" path
        || any (ext: hasSuffix ext (baseNameOf path)) [".zsh" ".bash" ".sh" ".ksh"];
      src = self;
    };
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
