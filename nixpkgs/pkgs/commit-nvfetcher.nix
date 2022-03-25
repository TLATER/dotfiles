{
  lib,
  stdenv,
  makeWrapper,
  pkgs,
  git,
  mktemp,
}:
stdenv.mkDerivation rec {
  pname = "commit-nvfetcher";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install commit-nvfetcher $out/bin
  '';
  nativeBuildInputs = [makeWrapper];
  wrapperPath = with lib; makeBinPath [pkgs.nvfetcher-bin git mktemp];
  postFixup = ''
    wrapProgram $out/bin/commit-nvfetcher \
        --prefix PATH : "${wrapperPath}"
  '';
}
