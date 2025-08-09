{ sources, stdenv }:
stdenv.mkDerivation {
  inherit (sources.catppuccin-alacritty) src pname version;
  installPhase = ''
    install -D --target-directory $out/share/alacritty/themes/ catppuccin-*.toml
  '';
}
