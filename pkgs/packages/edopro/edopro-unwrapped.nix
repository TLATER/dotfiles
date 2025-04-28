{
  stdenv,
  sources,

  # build deps
  premake5,

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
  '';
}
