{
  self,
  lib,
  stdenv,
  makeWrapper,
  git,
  mktemp,
  nvfetcher,
}:
stdenv.mkDerivation rec {
  pname = "commit-nvfetcher";
  version = "1.0";
  src = "${self}/home-config/dotfiles/bin";
  installPhase = ''
    mkdir -p $out/bin
    install commit-nvfetcher $out/bin
  '';
  nativeBuildInputs = [makeWrapper];
  wrapperPath = with lib; makeBinPath [nvfetcher git mktemp];
  postFixup = ''
    wrapProgram $out/bin/commit-nvfetcher \
        --prefix PATH : "${wrapperPath}"
  '';
}
