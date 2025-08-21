{ sources, stdenv }:
stdenv.mkDerivation {
  pname = "catppuccin-themes";
  version = "unknown";

  srcs = [
    (sources.catppuccin-alacritty.src.override { name = "catppuccin-alacritty"; })
    (sources.catppuccin-fuzzel.src.override { name = "catppuccin-fuzzel"; })
    (sources.catppuccin-i3.src.override { name = "catppuccin-i3"; })
  ];

  unpackPhase = ''
    runHook preUnpack

    for _src in $srcs; do
      cp -r "$_src" $(stripHash "$_src")
    done

    chmod -R u+rw *

    runHook postUnpack
  '';

  patchPhase = ''
    # Make backgrounds non-transparent
    find catppuccin-fuzzel/themes/ -name '*.ini' -exec sed -Ei 's/^(background=.*)dd$/\1ff/' {} +
  '';

  installPhase = ''
    install -D --target-directory $out/share/alacritty/themes/ catppuccin-alacritty/catppuccin-*.toml

    mkdir -p $out/share/{fuzzel,i3}
    cp -r catppuccin-fuzzel/themes $out/share/fuzzel
    cp -r catppuccin-i3/themes $out/share/i3
  '';
}
