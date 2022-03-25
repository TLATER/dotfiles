{
  lib,
  stdenv,
  git,
  python3,
  makeWrapper,
}: let
  inherit (lib) makeBinPath;
in
  stdenv.mkDerivation rec {
    pname = "system-update";
    version = "1.0";
    src = ../../dotfiles/bin;
    installPhase = ''
      mkdir -p $out/bin
      install system-update $out/bin
    '';
    nativeBuildInputs = [makeWrapper];
    buildInputs = [python3];
    postFixup = ''
      wrapProgram $out/bin/system-update \
          --prefix PATH : "${makeBinPath [git]}"
    '';
  }
