{ sources, stdenv }:
stdenv.mkDerivation {
  inherit (sources.phosphor-icons) pname version src;

  installPhase = ''
    mkdir -p $out/share/fonts/ttf

    for variant in src/*; do
        if [ -d "$variant" ]; then
            install -Dm 444 $variant/*.ttf $out/share/fonts/ttf/
        fi
    done
  '';
}
