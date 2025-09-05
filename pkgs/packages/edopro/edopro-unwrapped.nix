{
  stdenv,
  fetchFromGitHub,
  edopro-assets,

  # build deps
  premake5,
  imagemagick,

  # edopro deps
  bzip2,
  curl,
  flac,
  fmt,
  freetype,
  libevent,
  libgit2,
  libjpeg,
  libpng,
  libvorbis,
  nlohmann_json,
  noto-fonts-cjk-sans,
  openal,
  sqlite,

  irrlicht-edopro,
  ocgcore,
  luacxx,
}:
let
  font = "${noto-fonts-cjk-sans}/share/fonts/opentype/noto-cjk/NotoSansCJK-VF.otf.ttc";
in
stdenv.mkDerivation {
  pname = "edopro";
  version = "41.0.2";

  src = fetchFromGitHub {
    owner = "edo9300";
    repo = "edopro";
    rev = "90fcc7a546945f0b333e96190351e531f004298a";
    hash = "sha256-SLRd8z/zOAiJ/jxD1DUKVM0klMKBTy9RN3Q8n9oUCPQ=";
    fetchSubmodules = true;
  };

  nativeBuildInputs = [
    imagemagick
    premake5
  ];

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
    luacxx
    nlohmann_json
    ocgcore
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
      --architecture=x64 \
      --bundled-font='${font}' \
      --prebuilt-core='${ocgcore}/lib/'

    cd build
  '';

  installPhase = ''
    mkdir -p $out/{lib,bin}
    cp ../bin/x64/release/ygopro $out/bin/
    cp ../bin/x64/release/*.a $out/lib

    mkdir -p $out/share/applications/
    sed '/Path=/d
         s/Exec=.*/Exec=EDOPro/
         s/Icon=.*/Icon=EDOPro/' \
      ${edopro-assets}/config/io.github.edo9300.EDOPro.desktop.in \
      > $out/share/applications/io.github.edo9300.EDOPro.desktop

    mkdir -p $out/share/icons/hicolor/256x256/apps/
    magick ${edopro-assets}/textures/AppIcon.png \
      -resize 256x256 \
      $out/share/icons/hicolor/256x256/apps/EDOPro.png
  '';
}
