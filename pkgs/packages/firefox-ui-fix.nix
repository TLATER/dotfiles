{ sources, stdenvNoCC }:
stdenvNoCC.mkDerivation {
  inherit (sources.firefox-ui-fix) pname version src;

  installPhase = ''
    mkdir -p $out/
    cp -r user.js icons/ css/ $out/
  '';
}
