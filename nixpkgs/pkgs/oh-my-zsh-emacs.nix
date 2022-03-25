{
  sources,
  stdenv,
}:
stdenv.mkDerivation {
  inherit (sources.ohmyzsh) version src;
  pname = "oh-my-zsh-emacs";
  installPhase = ''
    mkdir -p $out/
    install plugins/emacs/emacs.plugin.zsh $out/
    install plugins/emacs/emacsclient.sh $out/
  '';
}
