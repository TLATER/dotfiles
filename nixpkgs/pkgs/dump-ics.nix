{
  stdenv,
  python310,
}:
stdenv.mkDerivation {
  pname = "dump-ics";
  version = "1.0";
  src = ../../dotfiles/bin;
  installPhase = ''
    mkdir -p $out/bin
    install dump-ics $out/bin
  '';
  buildInputs = [(python310.withPackages (pypkgs: with pypkgs; [ics]))];
}
