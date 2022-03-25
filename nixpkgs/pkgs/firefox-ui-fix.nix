{
  sources,
  stdenv,
}:
stdenv.mkDerivation rec {
  inherit (sources.firefox-ui-fix) pname version src;

  installPhase = ''
    mkdir -p $out/
    cp -r icons/ userChrome.css userContent.css $out/
  '';
}
