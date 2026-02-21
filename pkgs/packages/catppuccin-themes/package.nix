{
  stdenv,
  fetchFromGitHub,

  localLib,
  ast-grep,
  nix-prefetch-github,
}:
stdenv.mkDerivation {
  pname = "catppuccin-themes";
  version = "unknown";

  srcs = [
    (fetchFromGitHub {
      name = "catppuccin-alacritty";
      owner = "catppuccin";
      repo = "alacritty";
      rev = "f6cb5a5c2b404cdaceaff193b9c52317f62c62f7";
      hash = "sha256-H8bouVCS46h0DgQ+oYY8JitahQDj0V9p2cOoD4cQX+Q=";
    })

    (fetchFromGitHub {
      name = "catppuccin-fuzzel";
      owner = "catppuccin";
      repo = "fuzzel";
      rev = "879879da8a7dc58f173b4cd7987723fd19bef6d5";
      hash = "sha256-+/7lxQTRDZ0m+GAAFIjvFt8EXDeqZUtv0pLnNgaauZw=";
    })

    (fetchFromGitHub {
      name = "catppuccin-i3";
      owner = "catppuccin";
      repo = "i3";
      rev = "cd6b5017850084d5b40ef9f4eeac5a6d95779939";
      hash = "sha256-91GsedHF6xM1jmutZX/xdNtGFDrGerRSaRVh29CXt8U=";
    })
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

  passthru.updateScript = localLib.writeUpdateScript {
    packageToUpdate = "catppuccin-themes";

    utils = [
      ast-grep
      nix-prefetch-github
    ];

    script = ./update.nu;
  };
}
