{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "cap";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install cap $out/bin
  '';
  propagatedBuildInputs = [ scrot xorg.xprop ];
}
