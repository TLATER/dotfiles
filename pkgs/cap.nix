{
  self,
  lib,
  stdenv,
  scrot,
  xorg,
  makeWrapper,
}:
stdenv.mkDerivation rec {
  pname = "cap";
  version = "1.0";
  src = "${self}/home-config/dotfiles/bin";
  installPhase = ''
    mkdir -p $out/bin
    install cap $out/bin
  '';
  nativeBuildInputs = [makeWrapper];
  wrapperPath = with lib; makeBinPath [scrot xorg.xprop];
  postFixup = ''
    wrapProgram $out/bin/cap \
        --prefix PATH : "${wrapperPath}"
  '';
}
