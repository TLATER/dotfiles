{
  stdenv,
  sources,

  # build deps
  premake5,
  writeShellScriptBin,
  makeDesktopItem,
  symlinkJoin,

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
  lua5_4_compat,
  nlohmann_json,
  noto-fonts-cjk-sans,
  openal,
  sqlite,

  # Irrlicht deps
  wayland,
  libxkbcommon,
}:
let
  lua = lua5_4_compat;

  irrlicht-edopro = irrlicht.overrideAttrs (old: {
    inherit (sources.edopro-irrlicht) pname version src;
    buildInputs = old.buildInputs ++ [
      wayland
      libxkbcommon
    ];
  });

  ocgcore = stdenv.mkDerivation {
    inherit (sources.edopro) version src;
    pname = sources.edopro.pname + "-ocgcore";

    nativeBuildInputs = [ premake5 ];
    buildInputs = [ lua ];

    enableParallelBuilding = true;
    buildFlags = [ "verbose=true config=release ocgcoreshared" ];

    # nixpkgs' lua is compiled as C, but lua can also be compiled as
    # C++. Due to name mangling, a C-compiled lua will not be able to
    # link to a C++ project without some hoops, but edopro normally
    # builds it as a C++ library, so their code isn't used to this
    # edge case.
    #
    # Hence we need to change the included headers a bit.
    postPatch = ''
      sed -i 's/#include <lua.h>/#include <lua.hpp>/g' ocgcore/*.*
      sed -i '/#include <lualib.h>/d' ocgcore/*.*
      sed -i '/#include <lauxlib.h>/d' ocgcore/*.*
    '';

    preBuild = ''
      cd ocgcore
      premake5 gmake2 --lua-path='${lua}'
      cd build
    '';

    installPhase = ''
      mkdir -p $out/lib
      cp -r ../bin/release/* $out/lib/
    '';
  };

  font = "${noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc";
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
      lua
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
    buildFlags = [ "verbose=true config=release_x64 ygopro" ];

    preBuild = ''
      premake5 gmake2 \
        --sound=sfml \
        --no-joystick=true \
        --os=linux \
        --prebuilt-core='${ocgcore}/lib' \
        --architecture=x64 \
        --bundled-font='${font}' \
        --lua-path='${lua}'

      cd build
    '';

    installPhase = ''
      mkdir -p $out/{lib,bin}
      cp ../bin/x64/release/ygopro $out/bin/
      cp ../bin/x64/release/*.a $out/lib
    '';
  };

  edopro-script = writeShellScriptBin "EDOPro" ''
    set -eu
    EDOPRO_DIR="''${XDG_DATA_HOME:-~/.local/share}/edopro"

    if [ ! -d "''${EDOPRO_DIR}" ]; then
        mkdir -p "''${EDOPRO_DIR}"
        cp -r ${sources.edopro-distribution.src}/*/ "''${EDOPRO_DIR}/"
        find "''${EDOPRO_DIR}" -type d -exec chmod 700 {} +
        find "''${EDOPRO_DIR}" -type f -exec chmod 600 {} +
    fi

    exec ${edopro}/bin/ygopro -C "''${EDOPRO_DIR}" $@
  '';

  edopro-desktop = makeDesktopItem {
    name = "edopro-desktop";
    desktopName = "EDOPro";
    genericName = "Yu-Gi-Oh! simulator";
    icon = "EDOPro";
    exec = "EDOPro";
    # TODO(tlater): Add mime types - can use an XML file like [this
    # one](https://github.com/NixOS/nixpkgs/blob/a8b599256e0692225c2b476530d0a5b0f65e34ef/pkgs/applications/graphics/antimony/mimetype.xml)
    # to create the mime types, then add the `mimeTypes` attr here to
    # list the ones that associate to EDOPro.
    categories = [ "Game" ];
    keywords = [ "yugioh" ];
  };
in
symlinkJoin {
  name = "edopro-application";
  paths = [
    edopro
    edopro-script
    edopro-desktop
  ];

  postBuild = ''
    install -D ${sources.edopro-distribution.src}/textures/AppIcon.png \
        $out/share/icons/hicolor/1024x1024/apps/EDOPro.png
  '';
}
