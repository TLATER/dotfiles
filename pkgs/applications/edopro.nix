{
  lib,
  stdenv,
  sources,

  # build deps
  premake5,
  writeShellScriptBin,
  runCommandLocal,
  symlinkJoin,
  imagemagick,

  # edopro deps
  bzip2,
  curl,
  flac,
  fmt,
  freetype,
  irrlicht,
  libevent,
  libgit2,
  libjpeg,
  libpng,
  libvorbis,
  nlohmann_json,
  noto-fonts-cjk-sans,
  openal,
  sqlite,

  # Irrlicht deps
  wayland,
  egl-wayland,
  libxkbcommon,
}:
let
  font = "${noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc";

  irrlicht-edopro = irrlicht.overrideAttrs (old: {
    inherit (sources.edopro-irrlicht) pname version src;
    buildInputs = old.buildInputs ++ [
      wayland
      libxkbcommon
    ];

    preBuild = ''
      makeFlagsArray+=(sharedlib NDEBUG=1)
    '';
  });

  edopro = stdenv.mkDerivation {
    inherit (sources.edopro) pname version src;

    nativeBuildInputs = [ premake5 ];

    buildInputs = [
      bzip2
      curl
      flac
      fmt
      freetype
      irrlicht-edopro
      libevent
      libgit2
      libjpeg
      libpng
      libvorbis
      nlohmann_json
      openal
      sqlite
    ];

    # nixpkgs' gcc stack currently appears to not support LTO
    postPatch = ''
      sed -i '/LinkTimeOptimization/d' ./premake5.lua
    '';

    # Edopro normally builds irrlicht without a prefix in its include
    # dir
    NIX_CFLAGS_COMPILE = "-I ${irrlicht-edopro}/include/irrlicht/";

    enableParallelBuilding = true;
    buildFlags = [ "verbose=true config=release_x64 ygoprodll" ];

    preBuild = ''
      premake5 gmake2 \
        --sound=sfml \
        --no-joystick=true \
        --os=linux \
        --no-core \
        --architecture=x64 \
        --bundled-font='${font}'

      cd build
    '';

    installPhase = ''
      mkdir -p $out/{lib,bin}
      cp ../bin/x64/release/ygoprodll $out/bin/
      cp ../bin/x64/release/*.a $out/lib
    '';
  };

  edopro-script =
    let
      assetsToCopy = lib.concatStringsSep "," [
        "config"
        "deck"
        "COPYING.txt"
        "expansions"
        # We grab the ocgcore from the official release bundle, on
        # upstream suggestion; this is completely static and has no
        # dependencies, so it should be fine.
        "libocgcore.so"
        "lflists"
        "notices"
        "puzzles"
        "fonts"
        "script"
        "skin"
        "sound"
        "textures"
      ];
    in
    writeShellScriptBin "EDOPro" ''
      set -eu
      EDOPRO_DIR="''${XDG_DATA_HOME:-~/.local/share}/edopro"

      if [ ! -d "''${EDOPRO_DIR}" ]; then
          mkdir -p "''${EDOPRO_DIR}"
          cp -r ${sources.edopro-assets.src}/{${assetsToCopy}} "''${EDOPRO_DIR}/"

          find "''${EDOPRO_DIR}" -type d -exec chmod u=rwx,go-rwx {} +
          find "''${EDOPRO_DIR}" -type f -exec chmod u=rw,go-rwx {} +

          rm "''${EDOPRO_DIR}/config/io.github.edo9300.EDOPro.desktop.in"
      fi

      export LD_LIBRARY_PATH='${
        lib.makeLibraryPath [
          wayland
          egl-wayland
          libxkbcommon
        ]
      }';

      exec ${edopro}/bin/ygoprodll -C "''${EDOPRO_DIR}" $@
    '';

  edopro-desktop = runCommandLocal "io.github.edo9300.EDOPro.desktop" { } ''
    cp ${sources.edopro-assets.src}/config/io.github.edo9300.EDOPro.desktop.in desktop-template

    sed '/Path=/d' -i desktop-template
    sed 's/Exec=.*/Exec=EDOPro/' -i desktop-template
    sed 's/Icon=.*/Icon=EDOPro/' -i desktop-template

    install -D desktop-template $out/share/applications/io.github.edo9300.EDOPro.desktop
  '';
in
symlinkJoin {
  name = "edopro-application";
  paths = [
    edopro
    edopro-script
    edopro-desktop
  ];

  postBuild = ''
    mkdir -p $out/share/icons/hicolor/256x256/apps/
    ${imagemagick}/bin/magick \
        ${sources.edopro-assets.src}/textures/AppIcon.png \
        -resize 256x256 \
        $out/share/icons/hicolor/256x256/apps/EDOPro.png
  '';
}
