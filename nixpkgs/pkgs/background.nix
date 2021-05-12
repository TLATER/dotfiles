{ lib, stdenv, feh, makeWrapper }:

stdenv.mkDerivation rec {
  pname = "background";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install background $out/bin
  '';
  nativeBuildInputs = [ makeWrapper ];
  wrapperPath = with lib; makeBinPath [ feh ];
  postFixup = ''
    wrapProgram $out/bin/background \
        --prefix PATH : "${wrapperPath}"
  '';
}
