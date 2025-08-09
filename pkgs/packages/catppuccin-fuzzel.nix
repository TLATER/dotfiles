{ sources, stdenv }:
stdenv.mkDerivation {
  inherit (sources.catppuccin-fuzzel) src pname version;
  patchPhase = ''
    # Make backgrounds non-transparent
    find themes/ -name '*.ini' -exec sed -Ei 's/^(background=.*)dd$/\1ff/' {} +
  '';

  installPhase = ''
    mkdir -p $out/share/fuzzel/
    cp -r themes $out/share/fuzzel
  '';
}
