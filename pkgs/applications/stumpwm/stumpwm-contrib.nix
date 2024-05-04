{ sources, stdenv }:
stdenv.mkDerivation {
  inherit (sources.stumpwm-contrib) pname version src;
  installPhase = ''
    mkdir -p $out/share/stumpwm/modules
    cp -r * $out/share/stumpwm/modules
  '';
}
