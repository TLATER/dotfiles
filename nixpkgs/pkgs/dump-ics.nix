{ pkgs }:

with pkgs;

stdenv.mkDerivation {
  pname = "dump-ics";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install dump-ics $out/bin
  '';
  propagatedBuildInputs = [ python38 python38Packages.ics ];
}
