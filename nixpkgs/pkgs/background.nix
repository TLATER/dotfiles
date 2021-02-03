{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "background";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install background $out/bin
  '';
  propagatedBuildInputs = [ feh ];
}
