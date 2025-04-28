{
  sources,
  lib,

  symlinkJoin,
  runCommand,
  writeShellScriptBin,

  imagemagick,

  wayland,
  egl-wayland,
  libxkbcommon,

  edopro-unwrapped,
}:
let
  assetsToCopy = lib.concatStringsSep "," [
    "config"
    "deck"
    "COPYING.txt"
    "expansions"
    "lflists"
    "notices"
    "puzzles"
    "fonts"
    "script"
    "skin"
    "sound"
    "textures"
  ];

  libraryPath = lib.makeLibraryPath [
    wayland
    egl-wayland
    libxkbcommon
  ];

  edopro = writeShellScriptBin "EDOPro" ''
    set -eu
    EDOPRO_DIR="''${XDG_DATA_HOME:-~/.local/share}/edopro"

    if [ ! -d "$EDOPRO_DIR" ]; then
        mkdir -p "$EDOPRO_DIR"
        cp -r ${sources.edopro-assets.src}/{${assetsToCopy}} "''${EDOPRO_DIR}/"

        chmod -R go-rwx "$EDOPRO_DIR"

        rm "$EDOPRO_DIR/config/io.github.edo9300.EDOPro.desktop.in"
    fi

    export LD_LIBRARY_PATH='${libraryPath}';

    exec ${edopro-unwrapped}/bin/ygopro -C "''${EDOPRO_DIR}" $@
  '';

  desktopFile =
    runCommand "io.github.edo9300.EDOPro.desktop" { nativeBuildInputs = [ imagemagick ]; }
      ''
        cp ${sources.edopro-assets.src}/config/io.github.edo9300.EDOPro.desktop.in desktop-template

        sed '/Path=/d' -i desktop-template
        sed 's/Exec=.*/Exec=EDOPro/' -i desktop-template
        sed 's/Icon=.*/Icon=EDOPro/' -i desktop-template

        install -D desktop-template $out/share/applications/io.github.edo9300.EDOPro.desktop

        mkdir -p $out/share/icons/hicolor/256x256/apps/
        magick ${sources.edopro-assets.src}/textures/AppIcon.png \
          -resize 256x256 \
          $out/share/icons/hicolor/256x256/apps/EDOPro.png
      '';
in
symlinkJoin {
  name = "edopro";
  paths = [
    edopro
    desktopFile
  ];
}
