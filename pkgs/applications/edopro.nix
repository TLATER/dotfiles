{
  stdenv,
  sources,
  premake5,
  vcpkg,
  flac,
  libvorbis,
  openal,
  fetchpatch2,
  alsa-lib,
  libGL,
  mono,
  udev,
  xorg,
  wayland,
  libxkbcommon,
}:
stdenv.mkDerivation {
  inherit (sources.edopro) pname version src;
  TARGET_OS = "linux";
  TRAVIS_OS_NAME = "";
  VCPKG_ROOT = "${vcpkg}";

  patches = [
    # Fix a missing include in gcc13
    (fetchpatch2 {
      url = "https://github.com/edo9300/edopro/commit/0a0e378f4213c72dbcb1d2217a221a199f206e7b.patch";
      hash = "sha256-AK1cknHstFFBZAOrx7h5D1ZZ4UW2HYGfNU2pvDCTD/o=";
    })
  ];

  nativeBuildInputs = [ premake5 ];

  buildInputs = [
    flac
    libvorbis
    openal
    # alsa-lib
    # libGL
    # mono
    # udev
    # xorg.libX11
    # xorg.libXcursor
    # xorg.libXrandr
    # xorg.libXxf86vm
    # wayland
    # libxkbcommon
  ];

  postPatch = ''
    sed -i 's|./premake5|premake5|' ./travis/build.sh
  '';

  preBuild = ''
    bash -x ./travis/build.sh
    cd build
  '';
}
