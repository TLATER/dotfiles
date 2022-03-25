{
  lib,
  stdenv,
  python3,
  sops,
  makeWrapper,
}:
stdenv.mkDerivation rec {
  pname = "read-sops";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install read-sops $out/bin
  '';
  nativeBuildInputs = [makeWrapper];
  wrapperPath = with lib; makeBinPath [sops];
  buildInputs = [(python3.withPackages (pypkgs: with pypkgs; [xdg ruamel_yaml]))];
  postFixup = ''
    wrapProgram $out/bin/read-sops \
        --prefix PATH : "${wrapperPath}"
  '';
}
