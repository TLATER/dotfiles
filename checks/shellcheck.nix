{
  self,
  mkTest,
  lib,
  file,
  shellcheck,
}:
let
  inherit (lib.lists) any;
  inherit (lib.sources) cleanSourceWith;
  inherit (lib.strings) hasInfix hasSuffix;
in
mkTest {
  name = "lint-shellcheck";
  src = cleanSourceWith {
    filter =
      path: type:
      type == "directory"
      || hasInfix "dotfiles/zsh" path
      || hasInfix "pkgs/scripts" path
      || any (ext: hasSuffix ext (baseNameOf path)) [
        ".zsh"
        ".bash"
        ".sh"
        ".ksh"
      ];
    src = self;
  };
  checkInputs = [
    file
    shellcheck
  ];
  checkPhase = ''
    mkdir -p $out

    scripts=$(find pkgs/scripts -exec file --mime-type {} + \
      | grep -e 'text/x-shellscript' \
      | cut -d ':' -f 1)

    shellcheck $scripts | tee $out/test.log

    shellcheck home-config/dotfiles/zsh/*.zsh --shell=bash | tee -a $out/test.log
  '';
}
