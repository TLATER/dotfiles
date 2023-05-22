{
  sources,
  stdenv,
}:
stdenv.mkDerivation {
  inherit (sources.ohmyzsh) version src;
  pname = "oh-my-zsh-screen";
  installPhase = ''
    mkdir -p $out/
    install plugins/screen/screen.plugin.zsh $out/
  '';
}
