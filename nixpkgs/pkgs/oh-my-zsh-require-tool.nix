{
  sources,
  stdenv,
}:
stdenv.mkDerivation {
  inherit (sources.ohmyzsh) version src;
  pname = "oh-my-zsh-emacs";
  installPhase = ''
    mkdir -p $out/
    install tools/require_tool.sh $out/
    install tools/require_tool.sh $out/
  '';
}
